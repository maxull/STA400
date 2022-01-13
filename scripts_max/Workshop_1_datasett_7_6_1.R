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
