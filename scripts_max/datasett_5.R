##### datasett 5 analysis


library(foreign); library(tidyverse); library(psych);

data5 <- read.spss("data/Datasett 5_Fysisk aktivitet_ANOVA.sav", to.data.frame = TRUE)

data5 %>% 
        ggplot(aes(sample=STEPS_DAY))+
        geom_qq()+
        geom_qq_line()

data5 %>% 
        ggplot(aes(STEPS_DAY))+
        geom_histogram(binwidth = 600)
