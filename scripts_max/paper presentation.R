library(foreign); library(tidyverse); library(psych);library(readxl);library(gtsummary); library(gmodels)

tabel2 <- read_excel("data/Book1.xlsx", col_names = TRUE) %>% 
        print()



tabel2 %>% 
        tbl_summary(by = Group)

#data13 %>% 
        select(ASTMA,Stillesitting) %>% 
        tbl_summary(by = ASTMA,
                    statistic = c(Stillesitting) ~"{mean} ({sd})", 
                    digits = ~ 2) %>% 
        add_p(Stillesitting~"t.test", test.args = Stillesitting~list(var.equal = TRUE)) %>% 
        add_ci()
