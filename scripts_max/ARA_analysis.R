library(readxl); library(tidyr); library(tidyverse); library(ggplot2); library(dplyr); library(cowplot)
library(grid); library(gridExtra)

Results <- read_excel("data/ARA_Results.xlsx")

Results <- Results %>% 
        mutate(ARA_total = `ARA_20-21`+`ARA_21-22`)

######################################


### total regression plot


Results %>% 
        filter(ARA_total>0) %>% 
        ggplot(aes(Place, `ARA_total`))+
        geom_point(size = 1.2)+
        geom_smooth(method = lm, size = 1.5, color = "red")+
        xlim(75,0)+
        xlab("Plassering på NM 2022")+
        ylab("Antall ARANorge treninger siste to sesonger")+
        theme_bw(base_size = 16) -> tot_plot

ggsave("ARA_figures/total_plot.pdf", plot = tot_plot, 
       width =30,  height = 20, dpi = 600, units = "cm", device = cairo_pdf)


### plot for different disciplines


Results %>% 
        filter(`ARA_total`>0) %>% 
        filter(Discipline== c("SG", "SL", "GS")) -> tec

tec %>% 
        ggplot(aes(Place, `ARA_total`, group = Sex, fill= Sex))+
        geom_point(show.legend = FALSE)+
        geom_smooth(method = lm, se = FALSE, show.legend = FALSE, color = "purple", size = 1.5)+
        facet_grid(Discipline~Sex)+
        xlim(50,1)+
        xlab("Plassering på NM 2022")+
        ylab("Antall ARANorge treninger siste to sesonger")+
        ylim(-30,100)+
        theme(panel.background = element_rect(fill = "gray75",
                                                colour = "grey40",
                                                size = 1, linetype = "solid"))+
        theme(panel.grid.minor = element_blank())
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())#->disciplines_plot


##########################
### to do

        # different colors for each column (male green, female purple)
        # change that y lim ends at 1 place, not 0


#########################
### make separate plots, and stitch together with cowplot

# female

Female_SL <- Results %>% 
        filter(Sex == "Female",
               Discipline == "SL",
               ARA_total>0)

Female_GS <- Results %>% 
        filter(Sex == "Female",
               Discipline == "GS",
               ARA_total>0)

Female_SG <- Results %>% 
        filter(Sex == "Female",
               Discipline == "SG",
               ARA_total>0)

F_SL <- 
Female_SL %>% 
        ggplot(aes(Place, `ARA_total`))+
        geom_point(show.legend = FALSE)+
        geom_smooth(method = lm, se = FALSE, show.legend = FALSE, color = "darkturquoise", size = 1.5)+
        facet_grid(Discipline~Sex)+
        xlim(50,1)+
        ylab("Antall ARANorge treninger siste to sesonger")+
        coord_cartesian(xlim= c(50,0), expand = FALSE)+
        theme(panel.background = element_rect(fill = "mediumorchid1",
                                              colour = "grey40",
                                              size = 1, linetype = "solid"))+
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_line(color = "grey70"))+
        theme(axis.title.y = element_blank(),
              axis.title.x = element_blank())+
        theme(strip.background = element_rect(fill = "grey40"))+
        theme(text = element_text(color = "black"),
              strip.text = element_text(color = "white", size = 14),
              axis.text = element_text(color = "black"),
              axis.ticks = element_line(color = "white"),
              plot.background = element_rect(fill = "grey70"))

F_GS <-
Female_GS %>% 
        ggplot(aes(Place, `ARA_total`))+
        geom_point(show.legend = FALSE)+
        geom_smooth(method = lm, se = FALSE, show.legend = FALSE, color = "darkturquoise", size = 1.5)+
        facet_grid(Discipline~Sex)+
        coord_cartesian(xlim= c(50,0), expand = FALSE)+
        xlab("Plassering på NM 2022")+
        ylab("Antall ARANorge treninger siste to sesonger")+
        ylim(0,100)+
        theme(strip.text.x    = element_blank())+
        theme(panel.background = element_rect(fill = "mediumorchid2",
                                              colour = "grey40",
                                              size = 1, linetype = "solid"))+
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_line(color = "grey70"))+
        theme(axis.title.y = element_blank(),
              axis.title.x = element_blank())+
        theme(strip.background = element_rect(fill = "grey40"))+
        theme(text = element_text(color = "black"),
              strip.text = element_text(color = "white", size = 14),
              axis.text = element_text(color = "black"),
              axis.ticks = element_line(color = "white"),
              plot.background = element_rect(fill = "grey70"))

F_SG <- 
Female_SG %>% 
        ggplot(aes(Place, `ARA_total`))+
        geom_point(show.legend = FALSE)+
        geom_smooth(method = lm, se = FALSE, show.legend = FALSE, color = "darkturquoise", size = 1.5)+
        facet_grid(Discipline~Sex)+
        coord_cartesian(xlim= c(50,0), expand = FALSE)+
        xlab("Plassering på NM 2022")+
        ylab("Antall ARANorge treninger siste to sesonger")+
        ylim(0,100)+
        theme(strip.text.x    = element_blank())+
        theme(panel.background = element_rect(fill = "mediumorchid3",
                                              colour = "grey40",
                                              size = 1, linetype = "solid"))+
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_line(color = "grey70"))+
        theme(axis.title.y = element_blank(),
              axis.title.x = element_text(size = 14))+
        theme(strip.background = element_rect(fill = "grey40"))+
        theme(text = element_text(color = "black"),
              strip.text = element_text(color = "white", size = 14),
              axis.text = element_text(color = "black"),
              axis.ticks = element_line(color = "white"),
              plot.background = element_rect(fill = "grey70"))



female <- plot_grid(F_SL, F_GS, F_SG, ncol = 1)

### male 

Male_SL <- Results %>% 
        filter(Sex == "Male",
               Discipline == "SL",
               ARA_total>0)

Male_GS <- Results %>% 
        filter(Sex == "Male",
               Discipline == "GS",
               ARA_total>0)

Male_SG <- Results %>% 
        filter(Sex == "Male",
               Discipline == "SG",
               ARA_total>0)

M_SL <- 
        Male_SL %>% 
        ggplot(aes(Place, `ARA_total`))+
        geom_point(show.legend = FALSE)+
        geom_smooth(method = lm, se = FALSE, show.legend = FALSE, color = "blue", size = 1.5)+
        facet_grid(Discipline~Sex)+
        xlim(50,1)+
        ylab(" ")+
        coord_cartesian(xlim= c(50,0), expand = FALSE)+
        theme(panel.background = element_rect(fill = "skyblue1",
                                              colour = "grey40",
                                              size = 1, linetype = "solid"))+
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_line(color = "grey70"))+
        theme(axis.title.y = element_blank(),
              axis.title.x = element_blank())+
        theme(strip.background = element_rect(fill = "grey40"))+
        theme(text = element_text(color = "white"),
              strip.text = element_text(color = "white", size = 14),
              axis.text = element_text(color = "black"),
              axis.ticks = element_line(color = "white"),
              plot.background = element_rect(fill = "grey70"))

M_GS <-
        Male_GS %>% 
        ggplot(aes(Place, `ARA_total`))+
        geom_point(show.legend = FALSE)+
        geom_smooth(method = lm, se = FALSE, show.legend = FALSE, color = "blue", size = 1.5)+
        facet_grid(Discipline~Sex)+
        coord_cartesian(xlim= c(50,0), expand = FALSE)+
        xlab("Plassering på NM 2022")+
        ylab("Treninger med ARANorge siste to sesonger")+
        ylim(0,100)+
        theme(strip.text.x    = element_blank())+
        theme(panel.background = element_rect(fill = "skyblue2",
                                              colour = "grey40",
                                              size = 1, linetype = "solid"))+
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_line(color = "grey70"))+
        theme(axis.title.y = element_blank(),
              axis.title.x = element_blank())+
        theme(strip.background = element_rect(fill = "grey40"))+
        theme(text = element_text(color = "white"),
              strip.text = element_text(color = "white", size = 14),
              axis.text = element_text(color = "black"),
              axis.ticks = element_line(color = "white"),
              plot.background = element_rect(fill = "grey70"))

M_SG <- 
Male_SG %>% 
        ggplot(aes(Place, `ARA_total`))+
        geom_point(show.legend = FALSE)+
        geom_smooth(method = lm, se = FALSE, show.legend = FALSE, color = "blue", size = 1.5)+
        facet_grid(Discipline~Sex)+
        coord_cartesian(xlim= c(50,0), expand = FALSE)+
        xlab("Plassering på NM 2022")+
        ylab(" ")+
        ylim(0,100)+
        theme(strip.text.x    = element_blank())+
        theme(panel.background = element_rect(fill = "skyblue3",
                                              colour = "grey40",
                                              size = 1, linetype = "solid"))+
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_line(color = "grey70"))+
        theme(axis.title.y = element_blank(),
              axis.title.x = element_text(size = 14))+
        theme(strip.background = element_rect(fill = "grey40"))+
        theme(text = element_text(color = "black"),
              strip.text = element_text(color = "white", size = 14),
              axis.text = element_text(color = "black"),
              axis.ticks = element_line(color = "white"),
              plot.background = element_rect(fill = "grey70"))


male <- plot_grid(M_SL, M_GS, M_SG, ncol = 1)

y.grob <- textGrob("Treninger med ARA siste to sesonger", 
                   gp=gpar( col="black", fontsize=14), rot=90)

female <- grid.arrange(arrangeGrob(female, left = y.grob))


plot <- plot_grid(female, male, nrow = 1)

plot2 <- grid.arrange(arrangeGrob(plot, left = y.grob))

plot2 %>% 
        theme(plot.background = element_rect(fill = "grey70"))



ggsave("ARA_figures/disciplines2.pdf", plot = plot, 
       width =30,  height = 20, dpi = 600, units = "cm", device = cairo_pdf)




### plot for different ages

age_plot_tec<-
tec %>% 
        ggplot(aes(Place, `ARA_total`))+
        geom_point(show.legend = FALSE)+
        geom_smooth(method = lm, se = FALSE, show.legend = FALSE, size = 2)+
        facet_grid(~Age)+
        coord_cartesian(xlim= c(50,0), expand = FALSE)+
        xlab("Plassering på NM 2022")+
        ylab("Treninger med ARA siste to sesonger")+
        ylim(0, 100)+
        theme_bw(base_size = 16) + #-> age_plot_tec
        theme(strip.background = element_rect(fill = "grey40"),
              strip.text = element_text(color = "white"),
              plot.background = element_rect(fill = "grey70"),
              panel.background = element_rect(fill = "yellowgreen"),
              panel.grid.minor = element_blank())



############################
### regression analysis



tec %>% 
        filter(Sex == "Male") -> tec2
tec %>% 
        filter(Sex == "Female") -> tec3

regression.lm <- lm(tec3$Place~tec3$`ARA_21-22`) %>% 
        summary()


### distribution of residuals


regression.res <- resid(regression.lm)

plot(tec3$Place, regression.res)+
        abline(0,0)

ggsave("ARA_figures/age_tec_colors.pdf", plot = age_plot_tec, 
       width =30,  height = 14, dpi = 600, units = "cm", device = cairo_pdf)




#######################




Results %>% 
        filter(ARA_total>0) %>% 
        ggplot(aes(Place, `ARA_total`))+
        geom_point(show.legend = FALSE)+
        geom_smooth(method = lm, se = TRUE, show.legend = FALSE)+
        facet_grid(~Age)+
        xlim(50,1)+
        xlab("Plassering på NM 2022")+
        ylab("Antall ARANorge treninger siste to sesonger")+
        ylim(-25, 100)+
        theme_bw(base_size = 16) -> age_plot


ggsave("ARA_figures/age_all_disciplines.pdf", plot = age_plot, 
       width =30,  height = 14, dpi = 600, units = "cm", device = cairo_pdf)

#####################################################################
###             Correlations            #############################
######################################################################

### male and female correlation 20-21
        
        
Results_flt <- Results %>% 
        filter(`ARA_20-21`>0)
        
Results_flt %>% 
        filter(Sex == "Female") -> fr 
r_female21 <- cor(fr$Place, fr$`ARA_20-21`)

Results_flt %>% 
        filter(Sex == "Male") -> mr 
r_Male21 <- cor(mr$Place, mr$`ARA_20-21`)
        

### male and female correlation 21-22


Results_flt2 <- Results %>% 
        filter(`ARA_21-22`>0)

Results_flt %>% 
        filter(Sex == "Female") -> fr2 
r_female22 <- cor(fr$Place, fr$`ARA_21-22`)

Results_flt %>% 
        filter(Sex == "Male") -> mr2 
r_Male22 <- cor(mr$Place, mr$`ARA_21-22`)


### male and female correlation total


Results_flt3 <- Results %>% 
        filter(ARA_total>0)

Results_flt3 %>% 
        filter(Sex == "Female") -> fr3 
r_female_tot <- cor(fr3$Place, fr3$`ARA_total`)

Results_flt3 %>% 
        filter(Sex == "Male") -> mr3 
r_Male22_tot <- cor(mr3$Place, mr3$`ARA_total`)


### all gender correlations


r_tot <- cor(Results_flt3$Place,Results_flt3$ARA_total)
r_21 <- cor(Results_flt3$Place,Results_flt3$`ARA_20-21`)
r_22 <- cor(Results_flt3$Place,Results_flt3$`ARA_21-22`)


### SL correlation


Results_flt3 %>% 
        filter(Discipline == "SL") -> SL

SL %>% 
        filter(Sex == "Female") -> sl_r 
SL_r_female <- cor(sl_r$Place, sl_r$`ARA_total`)

SL %>% 
        filter(Sex == "Male") -> sl_r2 
SL_r_male <- cor(sl_r2$Place, sl_r2$`ARA_total`)



#### GS correlation


Results_flt3 %>% 
        filter(Discipline == "GS") -> GS

GS %>% 
        filter(Sex == "Female") -> GS_r 
GS_r_female <- cor(GS_r$Place, GS_r$`ARA_total`)

GS %>% 
        filter(Sex == "Male") -> GS_r2 
GS_r_male <- cor(GS_r2$Place, GS_r2$`ARA_total`)


#### SG correlation


Results_flt3 %>% 
        filter(Discipline == "SG") -> SG

SG %>% 
        filter(Sex == "Female") -> SG_r 
SG_r_female <- cor(SG_r$Place, SG_r$`ARA_total`)

SG %>% 
        filter(Sex == "Male") -> SG_r2 
SG_r_male <- cor(SG_r2$Place, SG_r2$`ARA_total`)

#####################################################################
#####################################################################
#####################################################################


### regressions


###############################

flt <- Results %>% 
        filter(`ARA_21-22`>0)


lm(flt$Place~flt$`ARA_21-22`)







cor(Results$Place, Results$`ARA_20-21`)

lm(Results$Place~Results$`ARA_20-21`) %>% 
        summary()

Results_flt <- Results %>% 
        filter(`ARA_20-21`>0)

cor(Results_flt$Place, Results_flt$`ARA_20-21`)

lm(Results_flt$Place~Results_flt$`ARA_20-21`) %>% 
        summary()


### SG

Results_flt %>% 
        filter(Discipline == "SG") %>% 
        ggplot(aes(Place, `ARA_20-21`, group = Sex, fill= Sex))+
        geom_point()+
        geom_smooth(method = lm)+
        xlim((max(Results$Place)),0)+
        theme_bw()

Results_SG <- Results_flt %>% 
        filter(Discipline == "SG")

cor(Results_SG$Place,Results_SG$`ARA_20-21`)

lm(Results_SG$Place~Results_SG$`ARA_20-21`) %>% 
        summary()

#############################################################
### ARA total

Results %>% 
        ggplot(aes(Place, ARA_total))+
        geom_point()+
        geom_smooth(method = lm)

cor(Results$Place, Results$ARA_total)

Results_filt <- Results %>% 
        filter(ARA_total>0)

Results_filt %>% 
        group_by(Sex) %>% 
        cor(Results_filt$Place, Results_filt$ARA_total)

Results_filt %>% 
        ggplot(aes(Place, ARA_total))+
        geom_point()+
        geom_smooth(method = lm)+
        xlim((max(Results_filt$Place)),0)

### by gender

Results_filt %>% 
        ggplot(aes(Place, ARA_total, fill = Age))+
        geom_point()+
        geom_smooth(method = lm, se = FALSE)+
        xlim((max(Results_filt$Place)),1)
        

Results_filt %>% 
        ggplot(aes(y =ARA_total))+
        geom_histogram(bins = 100)

#### U11

Results_11 <- Results_filt%>% 
        filter(Age == "U11")
        
Results_11 %>% 
        ggplot(aes(Place, ARA_total, fill = Age))+
        geom_point()+
        geom_smooth(method = lm)+
        xlim((max(Results_filt$Place)),1)

cor(Results_11$Place, Results_11$ARA_total)

### U12

Results_12 <- Results_filt%>% 
        filter(Age == "U12")

Results_12 %>% 
        ggplot(aes(Place, ARA_total, fill = Age))+
        geom_point()+
        geom_smooth(method = lm)+
        xlim((max(Results_filt$Place)),1)

cor(Results_12$Place, Results_12$ARA_total)

### U13

Results_13 <- Results_filt%>% 
        filter(Age == "U13")

Results_13 %>% 
        ggplot(aes(Place, ARA_total, fill = Age))+
        geom_point()+
        geom_smooth(method = lm)+
        xlim((max(Results_filt$Place)),1)

cor(Results_13$Place, Results_13$ARA_total)

### U14

Results_14 <- Results_filt%>% 
        filter(Age == "U14")

Results_14 %>% 
        ggplot(aes(Place, ARA_total, fill = Age))+
        geom_point()+
        geom_smooth(method = lm)+
        xlim((max(Results_filt$Place)),1)

cor(Results_14$Place, Results_14$ARA_total)

### U16

Results_16 <- Results_filt%>% 
        filter(Age == "U16")

Results_16 %>% 
        ggplot(aes(Place, ARA_total, fill = Age))+
        geom_point()+
        geom_smooth(method = lm)+
        xlim((max(Results_filt$Place)),1)

cor(Results_16$Place, Results_16$ARA_total)

par(mfrow=c(2,3))

### trening diary

ARA_log <- read.excel("data/ARA_treningslog.xlsx", sheet = "2020-21")

Participants <- ARA_log %>% 
        filter(Name == "Participants")

Participants %>% 
        print()
ARA
