library(readxl); library(tidyr); library(tidyverse); library(ggplot2)

Results <- read_excel("data/ARA_Results.xlsx")

Results <- Results %>% 
        mutate(ARA_total = `ARA_20-21`+`ARA_21-22`)

Results %>% 
        ggplot(aes(Place, `ARA_20-21`))+
        geom_point()+
        geom_smooth(method = lm)+
        xlim(100,0)

Results %>% 
        filter(`ARA_20-21`>0) %>% 
        ggplot(aes(Place, `ARA_20-21`, group = Sex, fill= Sex))+
        geom_point()+
        geom_smooth(method = lm)+
        xlim((max(Results$Place)),0)+
        theme_bw()
        

cor(Results$Place, Results$`ARA_20-21`)

lm(Results$Place~Results$`ARA_20-21`) %>% 
        summary()

Results_flt <- Results %>% 
        filter(`ARA_20-21`>0)

cor(Results_flt$Place, Results_flt$`ARA_20-21`)

lm(Results_flt$Place~Results_flt$`ARA_20-21`) %>% 
        summary()


### SG

Results_flt %>% 
        filter(Discipline == "SG") %>% 
        ggplot(aes(Place, `ARA_20-21`, group = Sex, fill= Sex))+
        geom_point()+
        geom_smooth(method = lm)+
        xlim((max(Results$Place)),0)+
        theme_bw()

Results_SG <- Results_flt %>% 
        filter(Discipline == "SG")

cor(Results_SG$Place,Results_SG$`ARA_20-21`)

lm(Results_SG$Place~Results_SG$`ARA_20-21`) %>% 
        summary()

#############################################################
### ARA total

Results %>% 
        ggplot(aes(Place, ARA_total))+
        geom_point()+
        geom_smooth(method = lm)

cor(Results$Place, Results$ARA_total)

Results_filt <- Results %>% 
        filter(ARA_total>0)

Results_filt %>% 
        group_by(Sex) %>% 
        cor(Results_filt$Place, Results_filt$ARA_total)

Results_filt %>% 
        ggplot(aes(Place, ARA_total))+
        geom_point()+
        geom_smooth(method = lm)+
        xlim((max(Results_filt$Place)),0)

### by gender

Results_filt %>% 
        ggplot(aes(Place, ARA_total, fill = Age))+
        geom_point()+
        geom_smooth(method = lm, se = FALSE)+
        xlim((max(Results_filt$Place)),1)
        

Results_filt %>% 
        ggplot(aes(y =ARA_total))+
        geom_histogram(bins = 100)

#### U11

Results_11 <- Results_filt%>% 
        filter(Age == "U11")
        
Results_11 %>% 
        ggplot(aes(Place, ARA_total, fill = Age))+
        geom_point()+
        geom_smooth(method = lm)+
        xlim((max(Results_filt$Place)),1)

cor(Results_11$Place, Results_11$ARA_total)

### U12

Results_12 <- Results_filt%>% 
        filter(Age == "U12")

Results_12 %>% 
        ggplot(aes(Place, ARA_total, fill = Age))+
        geom_point()+
        geom_smooth(method = lm)+
        xlim((max(Results_filt$Place)),1)

cor(Results_12$Place, Results_12$ARA_total)

### U13

Results_13 <- Results_filt%>% 
        filter(Age == "U13")

Results_13 %>% 
        ggplot(aes(Place, ARA_total, fill = Age))+
        geom_point()+
        geom_smooth(method = lm)+
        xlim((max(Results_filt$Place)),1)

cor(Results_13$Place, Results_13$ARA_total)

### U14

Results_14 <- Results_filt%>% 
        filter(Age == "U14")

Results_14 %>% 
        ggplot(aes(Place, ARA_total, fill = Age))+
        geom_point()+
        geom_smooth(method = lm)+
        xlim((max(Results_filt$Place)),1)

cor(Results_14$Place, Results_14$ARA_total)

### U16

Results_16 <- Results_filt%>% 
        filter(Age == "U16")

Results_16 %>% 
        ggplot(aes(Place, ARA_total, fill = Age))+
        geom_point()+
        geom_smooth(method = lm)+
        xlim((max(Results_filt$Place)),1)

cor(Results_16$Place, Results_16$ARA_total)

par(mfrow=c(2,3))

### trening diary

ARA_log <- read.excel("data/ARA_treningslog.xlsx", sheet = "2020-21")

Participants <- ARA_log %>% 
        filter(Name == "Participants")

Participants %>% 
        print()
ARA
