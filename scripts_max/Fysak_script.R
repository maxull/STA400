##### analysis of FYSAK datasett, Lesson 1

library(foreign); library(tidyverse); library(psych); library(cowplot); library(gtsummary)

### read SPSS file, .sav filetype
### library(foreign), must use argument "to.data.frame = TRUE" otherwise you get the data variables page


Fysak <- read.spss("data/Datasett 13 FYSAK (2).sav",to.data.frame = TRUE)

Fysak %>% 
        summary()

Fysak %>% 
        mutate(age = (2018 -F.år)) %>%
        select(ID,Kjønn,age,Steps_daily) %>% 
        print()-> Fysak2

Fysak2 %>% 
        describeBy("Kjønn")

Fysak2 %>% 
        mutate(old = as.factor(if_else(age<= 59, 1,0))) %>% 
        summary()


#### participant smmary table

Fysak2 %>% 
  select(-ID) %>% 
  tbl_summary(by = Kjønn,
              statistic = c(age,Steps_daily) ~"{mean}({sd})",
              missing = "no") %>% 
  add_overall()



### manually calculate %
### <=59 = 1806, >=60 = 1597
### % <=59 = 1806/3480*100 = 51.9%
### % >=60 = 1674/3480*100 = 48.1%
        
1806/3480*100
1674/3480*100

Fysak2 %>% 
        na.omit() %>% 
        ggplot(aes(Steps_daily))+
        geom_histogram()+
        facet_wrap(~Kjønn)
  
##############################################
#### create age groups

Fysak3 <- Fysak2 %>% 
        mutate(age_group = as.factor(cut(age, c(29,39,49,59,69,79,89,99), 
                               labels=c("30-39","40-49","50-59","60-69","70-79","80-89","90-99" )))) %>% 
        print()
        

### boxplot for all participants divided into age groups

boxplot(Fysak3$Steps_daily ~ Fysak3$age_group)

### boxplot for each gender

Fysak_male <- Fysak3 %>% 
        filter(Kjønn == "Mann")

Fysak_female <- Fysak3 %>% 
        filter(Kjønn == "Kvinne")

boxplot(Fysak_male$Steps_daily ~ Fysak_male$age_group)
boxplot(Fysak_female$Steps_daily ~ Fysak_female$age_group)


