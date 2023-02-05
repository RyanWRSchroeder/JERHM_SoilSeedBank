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

# Total Seed Bank Density 
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

# Native Annual Forbs
NativeAnnualForbSSB_ShrubEncroach_Microsite = summarySE(SSB_20.21_m2.Control, "Native_Annual_Forb", groupvars = c("Year", "Shrub_Encroach", "Shrub_Interspace")) # calculate mean & SE

BarPlot_NativeAnnualForbSSB_ShrubEncroach_Microsite_V = NativeAnnualForbSSB_ShrubEncroach_Microsite %>% ggplot(aes(y = Native_Annual_Forb, x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Native_Annual_Forb-se, ymax=Native_Annual_Forb+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(expression(Seed~density~(seeds~m^{-2})))+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 2, ncol = 1)+
  labs(title = "JERHM Native Annual Forb Seed Density (2020 & 2021)\nAveraged by Shrub Encroachment Level", fill = "Microsite")+
  theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 14),
        axis.text.y=element_text(color="black", size = 12),
        axis.title.x = element_text(color="black", size=14, margin = margin(10,0,0,0)),
        axis.title.y = element_text(color="black", size=14, margin = margin(0,5,0,0)),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = c(0.15,0.9),
        strip.background =element_rect(fill=NA))
BarPlot_NativeAnnualForbSSB_ShrubEncroach_Microsite_V

ggsave("3_output/results/BarPlot_NativeAnnualForbSSB_ShrubEncroach_Microsite_V_h8_w6_300dpi.jpeg",
       plot = BarPlot_NativeAnnualForbSSB_ShrubEncroach_Microsite_V, dpi = 300, height = 8, width = 6, units = "in" )

BarPlot_NativeAnnualForbSSB_ShrubEncroach_Microsite_H = NativeAnnualForbSSB_ShrubEncroach_Microsite %>% ggplot(aes(y = Native_Annual_Forb, x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Native_Annual_Forb-se, ymax=Native_Annual_Forb+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(expression(Seed~density~(seeds~m^{-2})))+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 1, ncol = 2)+
  labs(title = "JERHM Native Annual Forb Seed Density (2020 & 2021)\nAveraged by Shrub Encroachment Level", fill = "Microsite")+
  theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 14),
        axis.text.y=element_text(color="black", size = 12),
        axis.title.x = element_text(color="black", size=14, margin = margin(10,0,0,0)),
        axis.title.y = element_text(color="black", size=14, margin = margin(0,5,0,0)),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = c(0.15,0.9),
        strip.background =element_rect(fill=NA))
BarPlot_NativeAnnualForbSSB_ShrubEncroach_Microsite_H

ggsave("3_output/results/BarPlot_NativeAnnualForbSSB_ShrubEncroach_Microsite_H_h6_w8_300dpi.jpeg",
       plot = BarPlot_NativeAnnualForbSSB_ShrubEncroach_Microsite_H, dpi = 300, height = 6, width = 8, units = "in" )

# Native Perennial Grasses
NativePerennialGrassSSB_ShrubEncroach_Microsite = summarySE(SSB_20.21_m2.Control, "Native_Perennial_Grass", groupvars = c("Year", "Shrub_Encroach", "Shrub_Interspace")) # calculate mean & SE

BarPlot_NativePerennialGrassSSB_ShrubEncroach_Microsite_V = NativePerennialGrassSSB_ShrubEncroach_Microsite %>% 
  ggplot(aes(y = Native_Perennial_Grass, x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Native_Perennial_Grass-se, ymax=Native_Perennial_Grass+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(expression(Seed~density~(seeds~m^{-2})))+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 2, ncol = 1)+
  labs(title = "JERHM Native Perennial Grass Seed Density (2020 & 2021)\nAveraged by Shrub Encroachment Level", fill = "Microsite")+
  theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 14),
        axis.text.y=element_text(color="black", size = 12),
        axis.title.x = element_text(color="black", size=14, margin = margin(10,0,0,0)),
        axis.title.y = element_text(color="black", size=14, margin = margin(0,5,0,0)),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = c(0.15,0.9),
        strip.background =element_rect(fill=NA))
BarPlot_NativePerennialGrassSSB_ShrubEncroach_Microsite_V

ggsave("3_output/results/BarPlot_NativePerennialGrassSSB_ShrubEncroach_Microsite_V_h8_w6_300dpi.jpeg",
       plot = BarPlot_NativePerennialGrassSSB_ShrubEncroach_Microsite_V, dpi = 300, height = 8, width = 6, units = "in" )

BarPlot_NativePerennialGrassSSB_ShrubEncroach_Microsite_H = NativePerennialGrassSSB_ShrubEncroach_Microsite %>% 
  ggplot(aes(y = Native_Perennial_Grass, x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Native_Perennial_Grass-se, ymax=Native_Perennial_Grass+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(expression(Seed~density~(seeds~m^{-2})))+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 1, ncol = 2)+
  labs(title = "JERHM Native Perennial Grass Seed Density (2020 & 2021)\nAveraged by Shrub Encroachment Level", fill = "Microsite")+
  theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 14),
        axis.text.y=element_text(color="black", size = 12),
        axis.title.x = element_text(color="black", size=14, margin = margin(10,0,0,0)),
        axis.title.y = element_text(color="black", size=14, margin = margin(0,5,0,0)),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = c(0.15,0.9),
        strip.background =element_rect(fill=NA))
BarPlot_NativePerennialGrassSSB_ShrubEncroach_Microsite_H

ggsave("3_output/results/BarPlot_NativePerennialGrassSSB_ShrubEncroach_Microsite_H_h6_w8_300dpi.jpeg",
       plot = BarPlot_NativePerennialGrassSSB_ShrubEncroach_Microsite_H, dpi = 300, height = 6, width = 8, units = "in" )

# Shrubs
ShrubSSB_ShrubEncroach_Microsite = summarySE(SSB_20.21_m2.Control, "Shrub", groupvars = c("Year", "Shrub_Encroach", "Shrub_Interspace")) # calculate mean & SE

BarPlot_ShrubSSB_ShrubEncroach_Microsite_V = ShrubSSB_ShrubEncroach_Microsite %>% 
  ggplot(aes(y = Shrub, x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Shrub-se, ymax=Shrub+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(expression(Seed~density~(seeds~m^{-2})))+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 2, ncol = 1)+
  labs(title = "JERHM Shrub Seed Density (2020 & 2021)\nAveraged by Shrub Encroachment Level", fill = "Microsite")+
  theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 14),
        axis.text.y=element_text(color="black", size = 12),
        axis.title.x = element_text(color="black", size=14, margin = margin(10,0,0,0)),
        axis.title.y = element_text(color="black", size=14, margin = margin(0,5,0,0)),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = c(0.15,0.9),
        strip.background =element_rect(fill=NA))
BarPlot_ShrubSSB_ShrubEncroach_Microsite_V

ggsave("3_output/results/BarPlot_ShrubSSB_ShrubEncroach_Microsite_V_h8_w6_300dpi.jpeg",
       plot = BarPlot_ShrubSSB_ShrubEncroach_Microsite_V, dpi = 300, height = 8, width = 6, units = "in" )

BarPlot_ShrubSSB_ShrubEncroach_Microsite_H = ShrubSSB_ShrubEncroach_Microsite %>% 
  ggplot(aes(y = Shrub, x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Shrub-se, ymax=Shrub+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(expression(Seed~density~(seeds~m^{-2})))+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 1, ncol = 2)+
  labs(title = "JERHM Shrub Seed Density (2020 & 2021)\nAveraged by Shrub Encroachment Level", fill = "Microsite")+
  theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 14),
        axis.text.y=element_text(color="black", size = 12),
        axis.title.x = element_text(color="black", size=14, margin = margin(10,0,0,0)),
        axis.title.y = element_text(color="black", size=14, margin = margin(0,5,0,0)),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = c(0.15,0.9),
        strip.background =element_rect(fill=NA))
BarPlot_ShrubSSB_ShrubEncroach_Microsite_H

ggsave("3_output/results/BarPlot_ShrubSSB_ShrubEncroach_Microsite_H_h6_w8_300dpi.jpeg",
       plot = BarPlot_ShrubSSB_ShrubEncroach_Microsite_H, dpi = 300, height = 6, width = 8, units = "in" )

# Native Annual Grasses
NativeAnnualGrassSSB_ShrubEncroach_Microsite = summarySE(SSB_20.21_m2.Control, "Native_Annual_Grass", groupvars = c("Year", "Shrub_Encroach", "Shrub_Interspace")) # calculate mean & SE

BarPlot_NativeAnnualGrassSSB_ShrubEncroach_Microsite_V = NativeAnnualGrassSSB_ShrubEncroach_Microsite %>% 
  ggplot(aes(y = Native_Annual_Grass, x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Native_Annual_Grass-se, ymax=Native_Annual_Grass+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(expression(Seed~density~(seeds~m^{-2})))+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 2, ncol = 1)+
  labs(title = "JERHM Native Annual Grass Seed Density (2020 & 2021)\nAveraged by Shrub Encroachment Level", fill = "Microsite")+
  theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 14),
        axis.text.y=element_text(color="black", size = 12),
        axis.title.x = element_text(color="black", size=14, margin = margin(10,0,0,0)),
        axis.title.y = element_text(color="black", size=14, margin = margin(0,5,0,0)),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = c(0.15,0.9),
        strip.background =element_rect(fill=NA))
BarPlot_NativeAnnualGrassSSB_ShrubEncroach_Microsite_V

ggsave("3_output/results/BarPlot_NativeAnnualGrassSSB_ShrubEncroach_Microsite_V_h8_w6_300dpi.jpeg",
       plot = BarPlot_NativeAnnualGrassSSB_ShrubEncroach_Microsite_V, dpi = 300, height = 8, width = 6, units = "in" )

BarPlot_NativeAnnualGrassSSB_ShrubEncroach_Microsite_H = NativeAnnualGrassSSB_ShrubEncroach_Microsite %>% 
  ggplot(aes(y = Native_Annual_Grass, x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Native_Annual_Grass-se, ymax=Native_Annual_Grass+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(expression(Seed~density~(seeds~m^{-2})))+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 1, ncol = 2)+
  labs(title = "JERHM Native Annual Grass Seed Density (2020 & 2021)\nAveraged by Shrub Encroachment Level", fill = "Microsite")+
  theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 14),
        axis.text.y=element_text(color="black", size = 12),
        axis.title.x = element_text(color="black", size=14, margin = margin(10,0,0,0)),
        axis.title.y = element_text(color="black", size=14, margin = margin(0,5,0,0)),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = c(0.15,0.9),
        strip.background =element_rect(fill=NA))
BarPlot_NativeAnnualGrassSSB_ShrubEncroach_Microsite_H

ggsave("3_output/results/BarPlot_NativeAnnualGrassSSB_ShrubEncroach_Microsite_H_h6_w8_300dpi.jpeg",
       plot = BarPlot_NativeAnnualGrassSSB_ShrubEncroach_Microsite_H, dpi = 300, height = 6, width = 8, units = "in" )

# Native Perennial Forb
NativePerennialForbSSB_ShrubEncroach_Microsite = summarySE(SSB_20.21_m2.Control, "Native_Perennial_Forb", groupvars = c("Year", "Shrub_Encroach", "Shrub_Interspace")) # calculate mean & SE

BarPlot_NativePerennialForbSSB_ShrubEncroach_Microsite_V = NativePerennialForbSSB_ShrubEncroach_Microsite %>% 
  ggplot(aes(y = Native_Perennial_Forb, x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Native_Perennial_Forb-se, ymax=Native_Perennial_Forb+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(expression(Seed~density~(seeds~m^{-2})))+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 2, ncol = 1)+
  labs(title = "JERHM Native Perennial Forb Seed Density (2020 & 2021)\nAveraged by Shrub Encroachment Level", fill = "Microsite")+
  theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 14),
        axis.text.y=element_text(color="black", size = 12),
        axis.title.x = element_text(color="black", size=14, margin = margin(10,0,0,0)),
        axis.title.y = element_text(color="black", size=14, margin = margin(0,5,0,0)),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = c(0.15,0.9),
        strip.background =element_rect(fill=NA))
BarPlot_NativePerennialForbSSB_ShrubEncroach_Microsite_V

ggsave("3_output/results/BarPlot_NativePerennialForbSSB_ShrubEncroach_Microsite_V_h8_w6_300dpi.jpeg",
       plot = BarPlot_NativePerennialForbSSB_ShrubEncroach_Microsite_V, dpi = 300, height = 8, width = 6, units = "in" )

BarPlot_NativePerennialForbSSB_ShrubEncroach_Microsite_H = NativePerennialForbSSB_ShrubEncroach_Microsite %>% 
  ggplot(aes(y = Native_Perennial_Forb, x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Native_Perennial_Forb-se, ymax=Native_Perennial_Forb+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(expression(Seed~density~(seeds~m^{-2})))+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 1, ncol = 2)+
  labs(title = "JERHM Native Perennial Forb Seed Density (2020 & 2021)\nAveraged by Shrub Encroachment Level", fill = "Microsite")+
  theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 14),
        axis.text.y=element_text(color="black", size = 12),
        axis.title.x = element_text(color="black", size=14, margin = margin(10,0,0,0)),
        axis.title.y = element_text(color="black", size=14, margin = margin(0,5,0,0)),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = c(0.15,0.9),
        strip.background =element_rect(fill=NA))
BarPlot_NativePerennialForbSSB_ShrubEncroach_Microsite_H

ggsave("3_output/results/BarPlot_NativePerennialForbSSB_ShrubEncroach_Microsite_H_h6_w8_300dpi.jpeg",
       plot = BarPlot_NativePerennialForbSSB_ShrubEncroach_Microsite_H, dpi = 300, height = 6, width = 8, units = "in" )

# Introduced Perennial Grasses
IntroducedPerennialGrassSSB_ShrubEncroach_Microsite = summarySE(SSB_20.21_m2.Control, "Introduced_Perennial_Grass", groupvars = c("Year", "Shrub_Encroach", "Shrub_Interspace")) # calculate mean & SE

BarPlot_IntroducedPerennialGrassSSB_ShrubEncroach_Microsite_V = IntroducedPerennialGrassSSB_ShrubEncroach_Microsite %>% 
  ggplot(aes(y = Introduced_Perennial_Grass, x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Introduced_Perennial_Grass-se, ymax=Introduced_Perennial_Grass+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(expression(Seed~density~(seeds~m^{-2})))+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 2, ncol = 1)+
  labs(title = "JERHM Introduced Perennial Grass Seed Density (2020 & 2021)\nAveraged by Shrub Encroachment Level", fill = "Microsite")+
  theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 14),
        axis.text.y=element_text(color="black", size = 12),
        axis.title.x = element_text(color="black", size=14, margin = margin(10,0,0,0)),
        axis.title.y = element_text(color="black", size=14, margin = margin(0,5,0,0)),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = c(0.15,0.9),
        strip.background =element_rect(fill=NA))
BarPlot_IntroducedPerennialGrassSSB_ShrubEncroach_Microsite_V

ggsave("3_output/results/BarPlot_IntroducedPerennialGrassSSB_ShrubEncroach_Microsite_V_h8_w6_300dpi.jpeg",
       plot = BarPlot_IntroducedPerennialGrassSSB_ShrubEncroach_Microsite_V, dpi = 300, height = 8, width = 6, units = "in" )

BarPlot_IntroducedPerennialGrassSSB_ShrubEncroach_Microsite_H = IntroducedPerennialGrassSSB_ShrubEncroach_Microsite %>% 
  ggplot(aes(y = Introduced_Perennial_Grass, x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Introduced_Perennial_Grass-se, ymax=Introduced_Perennial_Grass+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(expression(Seed~density~(seeds~m^{-2})))+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 1, ncol = 2)+
  labs(title = "JERHM Introduced Perennial Grass Seed Density (2020 & 2021)\nAveraged by Shrub Encroachment Level", fill = "Microsite")+
  theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 14),
        axis.text.y=element_text(color="black", size = 12),
        axis.title.x = element_text(color="black", size=14, margin = margin(10,0,0,0)),
        axis.title.y = element_text(color="black", size=14, margin = margin(0,5,0,0)),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = c(0.15,0.9),
        strip.background =element_rect(fill=NA))
BarPlot_IntroducedPerennialGrassSSB_ShrubEncroach_Microsite_H

ggsave("3_output/results/BarPlot_IntroducedPerennialGrassSSB_ShrubEncroach_Microsite_H_h6_w8_300dpi.jpeg",
       plot = BarPlot_IntroducedPerennialGrassSSB_ShrubEncroach_Microsite_H, dpi = 300, height = 6, width = 8, units = "in" )

#### Composite Figure along Shrub Encroachment Gradient #### 

Stripped_BarPlot_TotalSSB_ShrubEncroach_Microsite_H = TotalSSB_ShrubEncroach_Microsite %>%
  ggplot(aes(y = Total, x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Total-se, ymax=Total+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(name = "Total\nSeed Density")+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 1, ncol = 2, )+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.text.y=element_text(color="black", size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color="black", size=20, margin = margin(0,5,0,0)),
        legend.position = "none",
        strip.background =element_rect(fill=NA),
        strip.text = element_text(color="black", size = 18)
        )
Stripped_BarPlot_TotalSSB_ShrubEncroach_Microsite_H

Stripped_BarPlot_NativeAnnualForbSSB_ShrubEncroach_Microsite_H = NativeAnnualForbSSB_ShrubEncroach_Microsite %>%
  ggplot(aes(y = Native_Annual_Forb, x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Native_Annual_Forb-se, ymax=Native_Annual_Forb+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(name = "Native\nAnnual Forbs")+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 1, ncol = 2, )+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.text.y=element_text(color="black", size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color="black", size=20, margin = margin(0,5,0,0)),
        legend.position = "none",
        strip.background =element_blank(),
        strip.text = element_blank()
        )
Stripped_BarPlot_NativeAnnualForbSSB_ShrubEncroach_Microsite_H

Stripped_BarPlot_NativePerennialGrassSSB_ShrubEncroach_Microsite_H = NativePerennialGrassSSB_ShrubEncroach_Microsite %>%
  ggplot(aes(y = Native_Perennial_Grass, x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Native_Perennial_Grass-se, ymax=Native_Perennial_Grass+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(name = "Native\nPerennial Grasses")+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 1, ncol = 2, )+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.text.y=element_text(color="black", size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color="black", size=20, margin = margin(0,5,0,0)),
        legend.position = "none",
        strip.background =element_blank(),
        strip.text = element_blank()
  )
Stripped_BarPlot_NativePerennialGrassSSB_ShrubEncroach_Microsite_H

Stripped_BarPlot_NativePerennialForbSSB_ShrubEncroach_Microsite_H = NativePerennialForbSSB_ShrubEncroach_Microsite %>%
  ggplot(aes(y = Native_Perennial_Forb, x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Native_Perennial_Forb-se, ymax=Native_Perennial_Forb+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(name = "Native\nPerennial Forbs")+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 1, ncol = 2, )+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.text.y=element_text(color="black", size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color="black", size=20, margin = margin(0,5,0,0)),
        legend.position = "none",
        strip.background =element_blank(),
        strip.text = element_blank()
  )
Stripped_BarPlot_NativePerennialForbSSB_ShrubEncroach_Microsite_H

Stripped_BarPlot_ShrubSSB_ShrubEncroach_Microsite_H = ShrubSSB_ShrubEncroach_Microsite %>%
  ggplot(aes(y = Shrub, x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Shrub-se, ymax=Shrub+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(name = "Shrubs")+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 1, ncol = 2, )+
  labs(fill = "Microsite")+
  theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 20),
        axis.text.y=element_text(color="black", size = 18),
        axis.title.x = element_text(color="black", size=18, margin = margin(10,0,0,0)),
        axis.title.y = element_text(color="black", size=20, margin = margin(0,5,0,0)),
        legend.text=element_text(size=18),
        legend.title = element_blank(),
        legend.position = c(0.15,0.75),
        strip.background =element_blank(),
        strip.text = element_blank()
  )
Stripped_BarPlot_ShrubSSB_ShrubEncroach_Microsite_H

library(ggpubr)
SRM_2023_Figure = plot_grid(Stripped_BarPlot_TotalSSB_ShrubEncroach_Microsite_H, Stripped_BarPlot_NativeAnnualForbSSB_ShrubEncroach_Microsite_H,
                       Stripped_BarPlot_NativePerennialGrassSSB_ShrubEncroach_Microsite_H, Stripped_BarPlot_NativePerennialForbSSB_ShrubEncroach_Microsite_H,
                       Stripped_BarPlot_ShrubSSB_ShrubEncroach_Microsite_H, align="v", ncol=1, nrow=5, hjust = 0.04)
SRM_2023_Figure = annotate_figure(SRM_2023_Figure,
                             left = text_grob(expression(Seed~Density~(seeds~m^{-2})), color = "black", size = 16, face = "bold", rot = 90))
SRM_2023_Figure

ggsave("3_output/results/Composite_BarPlot_SSB_ShrubEncroach_Microsite_H_h12_w8_300dpi.jpeg",
       plot = SRM_2023_Figure, dpi = 300, height = 12 , width = 8, units = "in" )

