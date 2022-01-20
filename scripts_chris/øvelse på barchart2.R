library(tidyverse)

data()
?starwars
View(starwars)

starwars%>%
        filter(hair_color == "black" | hair_color == "brown")%>%
        drop_na(sex)%>%
        
        ggplot(aes(hair_color, fill = sex)) + 
                ###first aes is always x-axis, fill is the Y axis. 
        
        geom_bar(position = "dodge", 
                        ###telling R what plot, dodge is a way to split your bars and not stacking them (try removing it to see), alpha is the transparency of the plot
                 
                 alpha = 1)+
        theme()+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        labs(title = "Tittelen til figuren din", 
             x = "Hair colour",
             y = "Number")

              