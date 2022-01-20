library(highcharter)
library(tidyverse)
library(patchwork)
library(scales)
library(reshape2)


library(readxl)%>%
        crap_slett_etter_bruk <- read_excel("C:/Users/chris/OneDrive/EXCEL/crap slett etter bruk.xlsx")
        
        crap_slett_etter_bruk%>%
                pivot_longer(cols = c(Social, Entertainment))%>%
                
                ggplot(aes(x=Age, y = value, color = name))+
                geom_bar(stat = "identity", position = "dodge")+
                theme()+
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())+
                labs(title = "MongoSATANfitteFIGUR", 
                     x = "Age",
                     y = "")

        
        
       


        

        