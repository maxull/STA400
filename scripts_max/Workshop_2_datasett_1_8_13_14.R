### workshop 2 

library(foreign); library(tidyverse); library(psych);library(readxl);library(gtsummary); library(gmodels)


data8 <- read.spss("data/Datasett 8_Specific_training_experiment.sav", to.data.frame = TRUE)

### 2.1 a) check normallity 
data8 %>% 
        ggplot(aes(sum_pre))+
        geom_density()+
        facet_wrap(~group)

### left skew, maybe not normally distributed

data8 %>% 
        ggplot(aes(s20m_pre))+
        geom_density()+
        facet_wrap(~group)

### normally distributed

data8 %>% 
        ggplot(aes(vj_pre))+
        geom_density()+
        facet_wrap(~group)

### not normally distributed

data8 %>% 
        ggplot(aes(sum_post))+
        geom_density()+
        facet_wrap(~group)

### left skew, maybe not normally distributed

data8 %>% 
        ggplot(aes(s20m_post))+
        geom_density()+
        facet_wrap(~group)

### meh

data8 %>% 
        ggplot(aes(vj_post))+
        geom_density()+
        facet_wrap(~group)

### not normally distributed

#################################
### checking the change variables

data8 %>% 
        ggplot(aes(sum_diff))+
        geom_density()+
        facet_wrap(~group)

### not normally distributed

data8 %>% 
        ggplot(aes(s20m_diff))+
        geom_density()+
        facet_wrap(~group)

### normally distributed... ish

data8 %>% 
        ggplot(aes(vj_diff))+
        geom_density()+
        facet_wrap(~group)

### normally distributed

data8 %>% 
        ggplot(aes(sample=sum_diff))+
        geom_qq()+
        geom_qq_line()

### not normally distributed

data8 %>% 
        ggplot(aes(sample=s20m_diff))+
        geom_qq()+
        geom_qq_line()

### normally distributed... ish

data8 %>% 
        ggplot(aes(sample=vj_diff))+
        geom_qq()+
        geom_qq_line()

### normally distributed

shapiro.test(data8$sum_diff)    #violates normality assumtion
shapiro.test(data8$s20m_diff)   #normal
shapiro.test(data8$vj_diff)     #normal

##############################################################
#########################################################################
### 2.1 b)
library(car) #levines test

leveneTest(s20m_diff~group, data = data8, center = mean) #non-significant p value -> equal variance
leveneTest(vj_diff~group, data = data8, center= mean)    #non-significant p value -> equal variance

### equal variance means i can use students t-test

data8 %>% 
        select(group,s20m_diff,vj_diff) %>% 
        tbl_summary(by = group,
                    statistic = c(s20m_diff,vj_diff) ~"{mean} ({sd})", 
                    digits = ~ 2) %>% 
        add_p(~"t.test", var.equal = TRUE) %>% 
        add_ci()
        ### issues with determining the t.test method

### manual  t-test parametric with equal variance between groups -> student's t-test

t.test(data8$s20m_diff~data8$group, var.equal = TRUE)
t.test(data8$vj_diff~data8$group, var.equal = TRUE)

###visualize

data8 %>% 
        ggplot(aes(group,s20m_diff, fill = group))+
        geom_boxplot()
data8 %>% 
        ggplot(aes(group,vj_diff, fill = group))+
        geom_boxplot()


#################################################################################
#############################################################################
### 2.2 t-test for independent groups
data13 <- read.spss("data/Datasett 13 FYSAK (2).sav", to.data.frame = TRUE)

### 2.2 a) I en tverrsnitt-studie har du rekruttert 3480 deltakere og målt hvor mange minutter 
#   de bruker stillesittende (stå, sitte eller ligge) med et akselerometer. Du har også 
#   spurt dem om de har hatt astma og om de har eller har hatt kreft. Formuler 
#   hypotese (-r) og bestem uavhengige og avhengig (-e) variabler.

### H1.1: Personer uten astma beveger seg mere enn personer med astma
### H0.1: personer uten astma beveger seg like mye som personer med astma

### H1.2: Personer med kreft beveger seg mindre enn personer uten kreft
### H0.2: Det er ingen forskjell i aktivitet mellom kreft og tidligere kreft pasienter og friske

### b) test hypotesene

### sjekke normalfordeling av variablene

data13 %>% 
        ggplot(aes(Stillesitting, fill = ASTMA))+
        geom_density()+
        facet_wrap(~ASTMA)

data13 %>% 
        ggplot(aes(Stillesitting, fill = KREFT))+
        geom_density()+
        facet_wrap(~KREFT)

# density distribution looks normal

data13 %>% 
        ggplot(aes(sample=Stillesitting, fill = ASTMA))+
        geom_qq()+
        geom_qq_line()+
        facet_wrap(~ASTMA)

data13 %>% 
        ggplot(aes(sample=Stillesitting, fill = KREFT))+
        geom_qq()+
        geom_qq_line()+
        facet_wrap(~KREFT)
# QQ looks ok

describeBy(data13$Stillesitting, group = data13$ASTMA)
describeBy(data13$Stillesitting, group = data13$KREFT)

# skew and kurtosis looks ok
# means and medians are close

# many N therefore i use kolmogorov smirnov normality test

data13na <- data13 %>% 
        select(ASTMA,KREFT,Steps_daily) %>% 
        na.omit()

ks.test(data13$ASTMA, data13$Stillesitting)
ks.test(data13$KREFT, data13$Stillesitting)
# not normal

### i like the distribution, even though KS.test is not normal

### checking variance

leveneTest(Stillesitting~ASTMA, data = data13, center= mean)    #non-significant p value -> equal variance
leveneTest(Stillesitting~KREFT, data = data13, center= mean)    #non-significant p value -> equal variance

leveneTest(data13$Stillesitting~data13$ASTMA, center = mean)    # alternatice script
# equal variance

### equal variance + normal distribution -> students t-test

t.test(data13$Stillesitting~data13$ASTMA, var.equal=TRUE)
t.test(data13$Stillesitting~data13$KREFT, var.equal=TRUE)

### no difference between asmatics p-value = 0.1154
### difference between canser with or with previous cancer p-value = 0.03532

### create table

tbl1 <- data13 %>% 
        select(ASTMA,Stillesitting) %>% 
        tbl_summary(by = ASTMA,
                    statistic = c(Stillesitting) ~"{mean} ({sd})", 
                    digits = ~ 2) %>% 
        add_p(Stillesitting~"t.test", test.args = Stillesitting~list(var.equal = TRUE)) %>% 
        add_ci()

tbl2 <- data13 %>% 
        select(KREFT,Stillesitting) %>% 
        tbl_summary(by = KREFT,
                    statistic = c(Stillesitting) ~"{mean} ({sd})", 
                    digits = ~ 2) %>% 
        add_p(Stillesitting~"t.test", test.args = Stillesitting~list(var.equal = TRUE)) %>% 
        add_ci()

tbl_merge(list(tbl1,tbl2),
          tab_spanner = c("ASTMA", "CANCER"))
