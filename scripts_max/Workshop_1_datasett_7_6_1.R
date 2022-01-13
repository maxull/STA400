#### workshop 1 STA400

### open datasett 7 and make new variables

library(foreign); library(tidyverse); library(psych);

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
        
library(gmodels)
        
CrossTable(data6$Gender,data6$Quarter,prop.chisq = FALSE, format = "SPSS", digits = 1)

### clean up the table

CrossTable(data6$Gender,data6$Quarter,prop.chisq = FALSE, format = "SPSS", digits = 1, prop.r =)

        
        
        
        