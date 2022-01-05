library(readxl); library(tidyverse)



###############################################################
### load data #################################################

Fitness_Males <- read_excel("./data/Datasett 1_Fitnesstests.xls.xls", sheet = "Males") %>% 
        print()

Fitness_Females <- read_excel("./data/Datasett 1_Fitnesstests.xls.xls", sheet = "Females") %>% 
        print()


###############################################################
### Explore  data #############################################

summary(Fitness_Females) 

summary(Fitness_Males)

##################################################################
#### normality checks ###########################################


Fitness_Females %>% 
        ggplot(aes(Stature))+
        geom_histogram()

Fitness_Females %>% 
        ggplot(aes(Body_Mass))+
        geom_histogram()

Fitness_Females %>% 
        ggplot(aes(Est_VO2_max))+
        geom_histogram()

############ here





Fitness_Females %>% 
        ggplot(aes(Stature))+
        geom_histogram()

summary(Fitness_Females) %>% 
        print()

Fitness_Males %>% 
        ggplot(aes(Body_Mass,Stature))+
        geom_point()

west_norm %>%
        ggplot(aes(timepoint, norm.expr, 
                   group = paste(subject, leg),
                   color = sets)) + 
        geom_point()  +
        geom_line()+
        facet_grid(target ~ sets)