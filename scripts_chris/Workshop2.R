
library(foreign); library(tidyverse); library(psych); library(readxl); library(gtsummary); library(gmodels); library(car)


data13 <- read.spss("data/Datasett 13 FYSAK (2).sav", to.data.frame = TRUE) 

### Oppgave 2.2

#### a) I en tverrsnitt-studie har du rekruttert 3480 deltakere og målt hvor mange minutter 
#   de bruker stillesittende (stå, sitte eller ligge) med et akselerometer. Du har også 
#   spurt dem om de har hatt astma og om de har eller har hatt kreft. Formuler 
#   hypotese (-r) og bestem uavhengige og avhengig (-e) variabler.

### H1.1: Personer uten astma beveger seg mere enn personer med astma
### H0.1: personer uten astma beveger seg like mye som personer med astma

### H1.2: Personer med kreft beveger seg mindre enn personer uten kreft
### H0.2: Det er ingen forskjell i aktivitet mellom kreft og tidligere kreft pasienter og friske

### b) test hypotesene

### sjekke normalfordeling av variablene


#### Densityplots to check visual distributions:

data13%>% 
        ##drop_na(ASTMA) %>% Kan brukes dersom det vetta faen jeg...

        ggplot(aes(Stillesitting, fill = ASTMA))+
        geom_density()+
        facet_wrap(~ASTMA)                              ##Denne brukes for å sammenligne variabler med store forskjeller i absolutte verdier
        

data13%>% 
        ggplot(aes(sample = Stillesitting, fill = ASTMA))+
        geom_qq()+
        geom_qq_line()+
        facet_wrap(~ASTMA)


        #### Variablees are normally distributed 


#### QQ plot to check assumptions:

data13 %>% 
        ggplot(aes(Stillesitting, fill = KREFT))+
        geom_density()+
        facet_wrap(~KREFT)

data13%>% 
        ggplot(aes(sample = Stillesitting, fill = KREFT))+
        geom_qq()+
        geom_qq_line()+
        facet_wrap(~KREFT)

        #### Variablees are normally distributed 


### Descriptives (means, medians, SD, skew and kurtosis):

describeBy(data13$Stillesitting, group = data13$ASTMA)
        #### Descriptives indicate normal distribution

describeBy(data13$Stillesitting, group = data13$KREFT)
        #### Descriptives indicate normal distribution


#### Statistical tests for normality:

### Shapiro-wilks for lower ( n < 50)
### Kolmogorow-smirnov for higher (n >50)

ks.test(data13$ASTMA, data13$Stillesitting)
ks.test(data13$KREFT, data13$Stillesitting)

shapiro.test(data13$Stillesitting)
        ##test suggest that data is NOT normally distributed, although these tests are very sensitive and therefore must be taken with a "pich of salt".


###Levene test tests 

leveneTest(data13$Stillesitting, group = data13$ASTMA, center = mean)

leveneTest(data13$Stillesitting, group = data13$KREFT, center = mean)

leveneTest(data13$Stillesitting~data13$ASTMA, center = mean) ###better way to write "group =" is "~".

        ## test suggest that the variance is equal, as the P is > 0.05


        t.test(data13$Stillesitting~data13$ASTMA, var.equal = TRUE)
                        ## t-test suggest no difference between the groups (p > 0.05)
        
        t.test(data13$Stillesitting~data13$KREFT, var.equal = TRUE)
                        ## t-test suggest there is a difference between the groups, whereby the group with ongoing or previous cancer are more sedentary  (p < 0.05)        


        ### CREATE TABLE
        
        tbl1<- data13 %>% 
                select(ASTMA,Stillesitting) %>% 
                tbl_summary(by = ASTMA, 
                            statistic = c(Stillesitting) ~"(mean) ({sd})",
                            digits = ~2) %>% 
                add_p(Stillesitting~"t.test", test.args = Stillesitting~list(var.equal = TRUE)) %>% 
                add_ci()

        tbl2 <- data13 %>% 
                select(KREFT,Stillesitting) %>% 
                tbl_summary(by = KREFT, 
                            statistic = c(Stillesitting) ~"(mean) ({sd})",
                            digits = ~2) %>% 
                add_p(Stillesitting~"t.test", test.args = Stillesitting~list(var.equal = TRUE)) %>% 
                add_ci() 
        
        tbl_merge(list(tbl1, tbl2),
                tab_spanner = c("ASTMA", "CANCER"))

                            
        