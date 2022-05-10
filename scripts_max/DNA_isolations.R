#### DNA isolation from sorting


library(ggplot2);library(tidyr); library(readxl); library(tidyverse); library(cowplot);library(grid); library(gridExtra)


data <- read_excel("data/DNA_isolation.xlsx", sheet = "Sheet2")


##########################
### methods: dissosiator vs. douncer



method1 <- data %>% 
        ggplot(aes(y = `Mean_(ng)`, x = Method, fill = Method))+
        geom_boxplot()+
        theme(legend.position = "none")+
        xlab(element_blank())
        

method2 <- data %>% 
        ggplot(aes(y = `Nucleic Acids (ng/ul)`, x = Method, fill = Method))+
        geom_boxplot()+
        theme(legend.position = "none")+
        xlab(element_blank())

method <- plot_grid(method2, method1)

y.grob <- textGrob("Douncer vs. GentleMACS Dissosiator", 
                   gp=gpar( col="black", fontsize=14))
                   
grid.arrange(arrangeGrob(method, top = y.grob))



################################
#### DNA isolated mouse vs. human



data %>% 
        ggplot(aes(y = `Nucleic Acids (ng/ul)`, color = group, fill = group))+
        geom_boxplot()

data_pre <- data %>% 
        filter(group == "pre")



data_post <- data %>% 
        filter(group == "post")




plot1 <- data_post %>% 
        ggplot(aes(y = `Nucleic Acids (ng/ul)`, x = Species,fill = Species))+
        geom_boxplot()+
        theme(legend.position = "none")+
        labs(title = "After sorting DNA ng/ul")+
        xlab(element_blank())

plot2 <- data_post %>% 
        ggplot(aes(y = `Mean_(ng)`,x = Species ,fill = Species))+
        geom_boxplot()+
        theme(legend.position = "none")+
        labs(title = "After sorting total DNA ng")+
        xlab(element_blank())

plot3 <- data_pre %>% 
        ggplot(aes(y = `Nucleic Acids (ng/ul)`, x = Species ,fill = Species))+
        geom_boxplot()+
        theme(legend.position = "none")+
        labs(title = "Before sorting DNA ng/ul")+
        xlab(element_blank())

plot4 <- data_pre %>% 
        ggplot(aes(y = `Mean_(ng)`, x = Species ,fill = Species))+
        geom_boxplot()+
        theme(legend.position = "none")+
        labs(title = "Before sorting total DNA ng")+
        xlab(element_blank())

plot_grid(plot3, plot1, plot4, plot2)

