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

##############################################################
#########################################################################
### 2.1 b)

data8 %>% 
        select(group,s20m_diff,vj_diff) %>% 
        tbl_summary(by = group,
                    statistic = c(s20m_diff,vj_diff) ~"{mean} ({sd})", 
                    digits = ~ 2) %>% 
        add_p() %>% 
        add_ci()
        ### p_value = wilcox.test script lower down

t.test(data8$s20m_diff~data8$group)
t.test(data8$vj_diff~data8$group)

wilcox.test(s20m_diff ~group, data = data8, exact = FALSE)
wilcox.test(vj_diff ~group, data = data8, exact = FALSE)




data8 %>% 
        ggplot(aes(group,s20m_diff, fill = group))+
        geom_boxplot()
data8 %>% 
        ggplot(aes(group,vj_diff, fill = group))+
        geom_boxplot()
