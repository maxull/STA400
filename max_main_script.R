library(readxl); library(tidyverse); library(dataPreparation)



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
#### normality checks female #####################################


Fitness_Females %>% 
        ggplot(aes(Stature))+
        geom_histogram()

Fitness_Females %>% 
        ggplot(aes(Body_Mass))+
        geom_histogram()

Fitness_Females %>% 
        ggplot(aes(Est_VO2_max))+
        geom_histogram()

shapiro.test(Fitness_Females$Stature)

shapiro.test(Fitness_Females$Body_Mass)

shapiro.test(Fitness_Females$Est_VO2_max)


Fitness_Males %>% 
        ggplot(aes(Stature))+
        geom_histogram()

Fitness_Males %>% 
        ggplot(aes(Body_Mass))+
        geom_histogram()

Fitness_Males %>% 
        ggplot(aes(Est_VO2_max))+
        geom_histogram()

shapiro.test(Fitness_Males$Stature)

shapiro.test(Fitness_Males$Body_Mass)

shapiro.test(Fitness_Males$Est_VO2_max)


Fitness_Females %>% 
        ggplot(aes(sample = Stature))+
        geom_qq()+
        geom_qq_line()


########################################################################################################
###### filter outliers >3sd      #######################################################################

Fitness_Females2 <- Fitness_Females %>% 
        na.omit() %>% 
        remove_sd_outlier(n_sigmas = 3)
        
##### filtering worked



Fitness_Females2 %>% 
        ggplot(aes(Body_Mass))+
        geom_histogram()

Fitness_Males2 <- remove_sd_outlier(Fitness_Males,cols = "Body_Mass" ,n_sigmas = 3)

Fitness_Males2 %>% 
        ggplot(aes(Body_Mass))+
        geom_density()

shapiro.test(Fitness_Males2$Body_Mass)

shapiro.test(Fitness_Females2$Body_Mass)




####################################################################################################
### even after filtering 3sd data is not normally distributed
### trying manual filtering

summary(Fitness_Females)
Fitness_Females3 <- Fitness_Females %>% 
        na.omit() %>% 
        filter(Body_Mass <= mean(Body_Mass)+(3*sd(Body_Mass))) %>% 
        filter(Body_Mass >= mean(Body_Mass)-(3*sd(Body_Mass))) %>% 
        filter(Stature <= mean(Stature) + (3*sd(Stature))) %>% 
        filter(Stature >= mean(Stature) - (3*sd(Stature))) %>% 
        filter(Est_VO2_max <= mean(Est_VO2_max) + (3*sd(Est_VO2_max))) %>% 
        filter(Est_VO2_max >= mean(Est_VO2_max) - (3*sd(Est_VO2_max))) 

summary(Fitness_Females3)

Fitness_Males3 <- Fitness_Males %>% 
        na.omit() %>% 
        filter(Body_Mass <= mean(Body_Mass)+(3*sd(Body_Mass))) %>% 
        filter(Body_Mass >= mean(Body_Mass)-(3*sd(Body_Mass))) %>% 
        filter(Stature <= mean(Stature) + (3*sd(Stature))) %>% 
        filter(Stature >= mean(Stature) - (3*sd(Stature))) %>% 
        filter(Est_VO2_max <= mean(Est_VO2_max) + (3*sd(Est_VO2_max))) %>% 
        filter(Est_VO2_max >= mean(Est_VO2_max) - (3*sd(Est_VO2_max))) 

### visualize change

Fitness_Females3 %>% 
        ggplot(aes(Stature,Body_Mass)) +
        geom_point()+
        geom_smooth(method=lm)



Fitness_Females %>% 
        ggplot(aes(Stature))+
        geom_histogram()

Fitness_Females3 %>% 
        ggplot(aes(Stature))+
        geom_density()







Fitness_Males2 %>% 
        ggplot(aes(Body_Mass,Stature))+
        geom_point()

west_norm %>%
        ggplot(aes(timepoint, norm.expr, 
                   group = paste(subject, leg),
                   color = sets)) + 
        geom_point()  +
        geom_line()+
        facet_grid(target ~ sets)