#### Data Visualization - JERHM SSB - SRM 2023 Project ####

#### Packages ####
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(esquisse)

#### Soil Seed Bank Data Visulaization #### 
## Bar Plots to visualize seed bank densities along the degradation gradient and between microsites 
# SSB_20.21_m2.Control = read.csv("2_incremental/JERHM_2020_2021_SoilSeedBankSpeciesMatrix_ControlOnly_lumped_m2_20230204.csv")

#### Bar Plots along the Shrub Encroachment Gradient (Low, Medium, High) #### 
library(Rmisc)
str(SSB_20.21_m2.Control)
TotalSSB_ShrubEncroach_Microsite = summarySE(SSB_20.21_m2.Control, "Total", groupvars = c("Year", "Shrub_Encroach", "Shrub_Interspace")) # calculate mean & SE

BarPlot_TotalSSB_ShrubEncroach_Microsite_V = TotalSSB_ShrubEncroach_Microsite %>% ggplot(aes(y = Total, x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Total-se, ymax=Total+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(expression(Seed~density~(seeds~m^{-2})))+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 2, ncol = 1)+
  labs(title = "JERHM Total Seed Density (2020 & 2021)\nAveraged by Shrub Encroachment Level", fill = "Microsite")+
  theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 14),
        axis.text.y=element_text(color="black", size = 12),
        axis.title.x = element_text(color="black", size=14, margin = margin(10,0,0,0)),
        axis.title.y = element_text(color="black", size=14, margin = margin(0,5,0,0)),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = c(0.15,0.9),
        strip.background =element_rect(fill=NA))
BarPlot_TotalSSB_ShrubEncroach_Microsite_V

ggsave("3_output/results/BarPlot_TotalSSB_ShrubEncroach_Microsite_V_h8_w6_300dpi.jpeg",
       plot = BarPlot_TotalSSB_ShrubEncroach_Microsite_V, dpi = 300, height = 8, width = 6, units = "in" )

BarPlot_TotalSSB_ShrubEncroach_Microsite_H = TotalSSB_ShrubEncroach_Microsite %>% ggplot(aes(y = Total, x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Total-se, ymax=Total+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(expression(Seed~density~(seeds~m^{-2})))+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 1, ncol = 2)+
  labs(title = "JERHM Total Seed Density (2020 & 2021)\n Averaged by Shrub Encroachment Level", fill = "Microsite")+
  theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 14),
        axis.text.y=element_text(color="black", size = 12),
        axis.title.x = element_text(color="black", size=14, margin = margin(10,0,0,0)),
        axis.title.y = element_text(color="black", size=14, margin = margin(0,5,0,0)),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = c(0.1,0.9),
        strip.background =element_rect(fill=NA))
BarPlot_TotalSSB_ShrubEncroach_Microsite_H

ggsave("3_output/results/BarPlot_TotalSSB_ShrubEncroach_Microsite_H_h6_w8_300dpi.jpeg",
       plot = BarPlot_TotalSSB_ShrubEncroach_Microsite_H, dpi = 300, height = 6, width = 8, units = "in" )


#### Bar Plots along the Ecological State Gradient (Low, Medium, High, High + Invaded)

