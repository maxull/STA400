### analyse Max dxa data


library(tidyverse);library(readxl);library(gtsummary); library(gmodels)

### read excel file

dxa <- read_excel("data/max_dxa.xlsx")

dxa %>% 
        summary()

dxa %>% 
        filter(Area=="Total") %>% 
        ggplot(aes(Date, Tissue_%fat)+
        geom_point()

dxa %>% 
        (Area== "Total")
dxa %>% 
        filter(Area=="Total") %>% 
        ggplot(aes(Date, `lean(g)`/1000))+
        geom_point()+
        labs(y="lean mass in KG",
             x = "")+
        theme_bw()

dxa %>% 
        filter(Area == "Left Leg", Area == "Right") %>% 
        print()
        ggplot(aes(Date,`lean(g)`))+
        geom_point()
        
        
        