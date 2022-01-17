#### workshop 1 STA400

### open datasett 7 and make new variables

library(foreign); library(tidyverse); library(psych);library(readxl)

### read spss file datasett 7

data7 <- read.spss("data/Datasett 7_Lag nye variabler.sav", to.data.frame = TRUE)

### 1.1 
### a) compute age as new variable
### b) mutate Height variable to meters

data<- data7 %>% 
        mutate(ALDER = (2019-F.år),
               HØYDE = (HØYDE/100),
               BMI = (VEKT/(HØYDE*HØYDE)))

data %>% 
        ggplot(aes(BMI, group = Kjønn, fill=Kjønn))+
        geom_density()+
        facet_wrap(~Kjønn)

summary(data)

### weird distribution, checking data

### HØYDE ok

data %>% 
        ggplot(aes(HØYDE))+
        geom_density()+
        facet_wrap(~Kjønn)

data %>% 
        ggplot(aes(sample=HØYDE))+
        geom_qq()+
        geom_qq_line()

###  VEKT ok ish, some high values

data %>% 
        ggplot(aes(VEKT))+
        geom_density()+
        facet_wrap(~Kjønn)

data %>% 
        ggplot(aes(sample=VEKT))+
        geom_qq()+
        geom_qq_line()

data %>% 
        ggplot(aes(VEKT,HØYDE))+
        geom_point()+
        geom_smooth(method=lm)+
        facet_wrap(~Kjønn)

### boxplot compare gender BMI

data %>% 
        ggplot(aes(Kjønn, BMI, fill = Kjønn))+
        geom_boxplot()


### compare BMI and income

data %>% 
        ggplot(aes(Brutto_inntekt, BMI, fill = Brutto_inntekt))+
        geom_boxplot()


###################################################################################
#####################################################################################
##################################################################################

### oppgave 1.2

### read data

data6 <- read.spss("data/Datasett 6_Grand Slam Tennis.sav", to.data.frame = TRUE)


### how many male and female players

summary(data6)

### female = 183
### male = 193

### % male and female

183/(183+193)*100 #female
193/(183+193)*100 #male

typeof(data6$Quarter)

data6 %>% 
        ggplot(aes(Quarter, fill=Quarter))+
        geom_bar()+
        facet_wrap(~Gender)

#### trying to use crosstable from spss as performed in the example solution
####
#### works
        
library(gtsummary); library(gmodels)
        
CrossTable(data6$Gender,data6$Quarter,prop.chisq = FALSE, format = "SPSS", digits = 1) 

data6 %>% 
        select(Gender, Quarter) %>% 
        tbl_summary(by=Gender) %>% 
        add_overall()

#############################################################################
###############################################################################
##########################################################################

### oppgave 1.3 

### read datasett fitnesstests and combine data into one datasett

data1m <- read_excel("data/Datasett 1_Fitnesstests.xls.xls", sheet = "Males") %>% 
        print()

data1f <- read_excel("data/Datasett 1_Fitnesstests.xls.xls", sheet = "Females") %>% 
        print()

data1m %>% 
        mutate(Gender = as.character("Male")) %>% 
        print()-> data1M

data1F <- data1f %>% 
        mutate(Gender = as.character("Female")) 


data1 <- bind_rows(data1F,data1M) %>% 
        print()

### explore data means

data1 %>% 
        na.omit() %>% 
        tbl_summary(by = "Gender",
                    statistic = c(Stature, Body_Mass, Est_VO2_max) ~"{mean} ({sd})")

data1 %>% 
        filter(Gender == "Female") %>%  
        summary()

data1 %>% 
        filter(Gender == "Male") %>%  
        summary()

describeBy(data1, data1$Gender)

### explore data visually

data1 %>% 
        ggplot(aes(Stature, fill = Gender))+
        geom_density()+
        facet_wrap(~Gender)

data1 %>% 
        ggplot(aes(Body_Mass, fill = Gender))+
        geom_density()+
        facet_wrap(~Gender)

data1 %>% 
        ggplot(aes(Est_VO2_max, fill = Gender))+
        geom_density()+
        facet_wrap(~Gender)

data1 %>% 
        ggplot(aes(Stature, Body_Mass, group = Gender, fill = Gender))+
        geom_point()+
        geom_smooth(method=lm)+
        facet_wrap(~Gender)

### we have som extreme values, try to filter and reanalyse
### remove 3sd values

library(dataPreparation)

data1_filtered <- data1 %>% 
        na.omit() %>% 
        remove_sd_outlier(n_sigmas = 3)

describeBy(data1_filtered, data1_filtered$Gender)

data1_filtered %>% 
        ggplot(aes(Stature, Body_Mass, fill = Gender,))+
        geom_point()+
        geom_smooth(method=lm)+
        facet_wrap(~Gender)

data1_filtered %>% 
        ggplot(aes(Stature, fill = Gender))+
        geom_density()+
        facet_wrap(~Gender)

data1_filtered %>% 
        ggplot(aes(Body_Mass, fill = Gender))+
        geom_density()+
        facet_wrap(~Gender)

data1_filtered %>% 
        ggplot(aes(Est_VO2_max, fill = Gender))+
        geom_density()+
        facet_wrap(~Gender)


### compare table output of filtered and unfiltered data

tbl_1 <- data1 %>% 
        tbl_summary(by = "Gender",
                    statistic = c(Stature, Body_Mass, Est_VO2_max) ~"{mean} ({sd})", 
                    digits = ~2) %>% 
        add_p(vars = TRUE) %>% 
        add_ci()

tbl_2 <- data1_filtered %>% 
        tbl_summary(by = "Gender",
                    statistic = c(Stature, Body_Mass, Est_VO2_max) ~"{mean} ({sd})", 
                    digits = ~ 2) %>% 
        add_p(vars=TRUE) %>% 
        add_ci()

tbl_merge(list(tbl_1,tbl_2),
          tab_spanner = c("Raw data", "Filtered for >3SD"))

### means are quite similar, while SD are much lower after filtering out extreme values
###
### in this case i think i would use parametric and non-parametric tests, and report if
### the tests return differing results

### my own tests

### test of difference between groups (gender)
### parametric and non-parametric to determine if different

### check variance to determine best test of difference between independent groups


var(data1$Stature[data1$Gender == "Male"])
var(data1$Stature[data1$Gender == "Female"], na.rm = TRUE)

var(data1$Body_Mass[data1$Gender == "Male"])
var(data1$Body_Mass[data1$Gender == "Female"])

var(data1$Est_VO2_max[data1$Gender == "Male"])
var(data1$Est_VO2_max[data1$Gender == "Female"])

### all variances are differente -> t.test must assume unequal variances = Welch's t-test

t.test(data1$Stature~data1$Gender)

t.test(data1$Stature~data1$Gender)

t.test(data1$Est_VO2_max~data1$Gender)

t.test(data1_filtered$Stature~data1_filtered$Gender)

t.test(data1$Stature~data1$Gender)

t.test(data1$Est_VO2_max~data1$Gender)










