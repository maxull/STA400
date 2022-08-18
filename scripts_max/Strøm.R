library(tidyverse);library(ggplot2); library(rjson); library(plyr);library(data.table)

https://nvebiapi.nve.no/swagger/index.html

mydata <- fromJSON(file = "C:/Users/maxul/Downloads/response_1660809672599.json", simplify = TRUE)


data <- rbindlist(mydata)

data <- data[with(data, order(omrnr,iso_aar,iso_uke)),]

data %>% 
        mutate(time = paste(data$iso_aar,data$iso_uke, sep = "_"))

data$iso_uke <- as.numeric(data$iso_uke)
data$iso_aar <- as.numeric(data$iso_aar)

data$iso_aar <- factor(data$iso_aar)

data %>% 
        na.omit() %>% 
        filter(omrnr == "1") %>% 
        ggplot(aes(x = iso_uke, y = fyllingsgrad, color = iso_aar))+
        geom_smooth(se = FALSE)


data %>% 
        filter(iso_aar > 2015) -> data2010

data2010$iso_aar <- factor(data2010$iso_aar)
data2010 %>% 
        na.omit() %>% 
        filter(omrnr == "1") %>% 
        ggplot(aes(x = iso_uke, y = fyllingsgrad, color = iso_aar))+
        geom_smooth()

library(shiny)

runExample("08_html")

data %>% 
        filter(iso_aar > 2021)-> data2022

