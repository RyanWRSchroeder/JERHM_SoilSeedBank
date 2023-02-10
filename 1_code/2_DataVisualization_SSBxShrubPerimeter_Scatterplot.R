#### Data Visualization - JERHM SSB - SRM 2023 Project ####

#### Packages ####
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(esquisse)

#### Soil Seed Bank Data Visulaization #### 
## Scatter Plots to visualize if the size of the shrub islands influence the density of the 
# seed bank under the shrub islands 
# SSB_20.21_m2.SHRUB.Control.Join 

str(SSB_20.21_m2.SHRUB.Control.Join)

esquisser()

library(ggplot2)
library(ggpubr)

#Scatter.TotalxPerimeter.ShrubControl 

#Overall scatterplot 
Scatterplot.ShrubSSBxPerimeter.Control = SSB_20.21_m2.SHRUB.Control.Join %>% ggplot(aes(x = Perimeter_m, y = Total)) +
           geom_point(shape = "circle", size = 2L, color = "#112446")+
           geom_smooth(method = "lm")+
           scale_y_continuous(expression(Seed~density~(seeds~m^{-2})))+
           scale_x_continuous(expression(Shrub~Island~Perimeter~(m)))+
           coord_cartesian(xlim = c(5,35), ylim = c(0,7500))+
           theme_bw()+ 
            stat_cor(aes(label = paste(..p.label.., sep = "~`,`~")),
                     label.x = 25,label.y = 6000, p.accuracy = 0.001)+
           theme(
             axis.title.y = element_text(size = 18L), 
             axis.title.x = element_text(size = 18L)
             )
ggsave("3_output/results/ScatterPlot_ShrubSSBxPerimeter_h6_w6_300dpi.jpeg",
       plot = Scatterplot.ShrubSSBxPerimeter.Control, dpi = 300, height = 6, width = 6, units = "in" )

## Group by shrub encroachment level
SSB_20.21_m2.SHRUB.Control.Join %>% ggplot(aes(x = Perimeter_m, y = Total, color = Shrub_Encroach)) +
  geom_point(shape = "circle", size = 2L)+
  geom_smooth(method = "lm")+
  scale_y_continuous(expression(Seed~density~(seeds~m^{-2})))+
  scale_x_continuous(expression(Shrub~Island~Perimeter~(m)))+
  coord_cartesian(xlim = c(5,35), ylim = c(0,7500))+
  theme_bw()+ 
  stat_cor(aes(label = paste(..p.label.., sep = "~`,`~")),
           label.x = 25,label.y = c(6300, 6000, 5700), p.accuracy = 0.001)+
  theme(
    axis.title.y = element_text(size = 18L), 
    axis.title.x = element_text(size = 18L),
    legend.position = "top",
    legend.title = element_blank()
  )
         