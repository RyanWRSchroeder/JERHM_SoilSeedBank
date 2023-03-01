#### Data Visualization - JERHM Veg - ESA 2023 Project ####

#### Packages ####
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(esquisse)

VegSpRich = read.csv("0_data/JERHM_Vegetation/SppRich_AllYears_08Feb23.csv")
SSB_SiteLvl_20.21_m2 = read.csv("2_incremental/JERHM_2020_2021_SiteLevel_SoilSeedBankSpeciesMatrix_lumped_m2_20230228.csv")
str(VegSpRich)
VegSpRich$Plot = as.factor(VegSpRich$Plot)
VegSpRich$Year = as.factor(VegSpRich$Year)
VegSpRich$PlotYear = as.factor(VegSpRich$PlotYear)
VegSpRich$Treatment = as.factor(VegSpRich$Treatment)
VegSpRich$EcolState = as.factor(VegSpRich$EcolState)
VegSpRich$Shrub_Encroach = as.factor(VegSpRich$Shrub_Encroach)
VegSpRich$Shrub_Encroach = factor(VegSpRich$Shrub_Encroach, levels = c("Low", "Medium", "High"))
str(VegSpRich)

str(SSB_SiteLvl_20.21_m2)
SSB_SiteLvl_20.21_m2$Year = as.factor(SSB_SiteLvl_20.21_m2$Year)
SSB_SiteLvl_20.21_m2$Block_Num = as.factor(SSB_SiteLvl_20.21_m2$Block_Num)
SSB_SiteLvl_20.21_m2$Treatment = as.factor(SSB_SiteLvl_20.21_m2$Treatment)
SSB_SiteLvl_20.21_m2$EcolState = as.factor(SSB_SiteLvl_20.21_m2$EcolState)
SSB_SiteLvl_20.21_m2$Shrub_Encroach = as.factor(SSB_SiteLvl_20.21_m2$Shrub_Encroach)
SSB_SiteLvl_20.21_m2$Shrub_Encroach = factor(SSB_SiteLvl_20.21_m2$Shrub_Encroach, levels = c("Low", "Medium", "High"))
str(SSB_SiteLvl_20.21_m2)

library(Rmisc)
VegSpRich_Summary = summarySE(VegSpRich, "SppRich", groupvars = c("Year", "Treatment", "Shrub_Encroach"))
esquisser()

library(ggplot2)

DotPlot_Veg_Richness_20.21 = VegSpRich_Summary %>%
  filter(!(Year %in% "2022")) %>%
  ggplot() +
 aes(x = Shrub_Encroach, y = SppRich, colour = Treatment) +
 geom_point(shape = "circle", 
 size = 6, position = position_dodge(.9)) +
  geom_errorbar(aes(ymin=SppRich-se, ymax=SppRich+se), width=0.25, position=position_dodge(.9))+
 scale_color_brewer(palette = "Dark2", direction = 1) +
  scale_y_continuous(name = "Abv. Veg. Species Richness") +
  scale_x_discrete(name = "Shrub Encroachment Level") +
 theme_bw() +
 facet_wrap(vars(Year))+
  theme(legend.position = "top")

library(vegan)
library(dplyr)
SSB_20.21_SpRich = SSB_20.21_m2[,c(1:71)]
str(SSB_20.21_SpRich)
SSB_20.21_SpRich$Richness = specnumber(SSB_20.21_SpRich[,c(9:71)]) # calculate species richness
SSB_20.21_SpRich$H = diversity(SSB_20.21_SpRich[,c(9:71)]) # calculate Shannon-Wiener H

library(Rmisc)
SSBSpRich_Summary = summarySE(SSB_20.21_SpRich, "Richness", groupvars = c("Year", "Treatment", "Shrub_Encroach"))
SSBSpRich_Summary

DotPlot_SSB_Richness_20.21 = ggplot(SSBSpRich_Summary) +
  aes(x = Shrub_Encroach, y = Richness, colour = Treatment) +
  geom_point(shape = "circle", 
             size = 6, position = position_dodge(.9)) +
  geom_errorbar(aes(ymin=Richness-se, ymax=Richness+se), width=0.25, position=position_dodge(.9))+
  scale_color_brewer(palette = "Dark2", direction = 1) +
  scale_y_continuous(name = "Seed Bank Species Richness") +
  scale_x_discrete(name = "Shrub Encroachment Level") +
  theme_bw() +
  facet_wrap(vars(Year))+
  theme(legend.position = "top")

Richness_Composite_Fig = plot_grid(DotPlot_Veg_Richness_20.21, DotPlot_SSB_Richness_20.21,
                            align="h", ncol=2, nrow=1, hjust = 0.04)
Richness_Composite_Fig
ggsave("3_output/results/Composite_DotPlot_AbvVegSSB-Richness_ShrubEncroach_Microsite_H_h8_w12_300dpi.jpeg",
       plot = Richness_Composite_Fig, dpi = 300, height = 8 , width = 12, units = "in" )


SSB.H_Summary = summarySE(SSB_20.21_SpRich, "H", groupvars = c("Year", "Treatment", "Shrub_Encroach"))
SSB.H_Summary

DotPlot_SSB_H_20.21 = ggplot(SSB.H_Summary) +
  aes(x = Shrub_Encroach, y = H, colour = Treatment) +
  geom_point(shape = "circle", 
             size = 6, position = position_dodge(.9)) +
  geom_errorbar(aes(ymin=H-se, ymax=H+se), width=0.25, position=position_dodge(.9))+
  scale_color_brewer(palette = "Dark2", direction = 1) +
  scale_y_continuous(name = "Seed Bank Species Diversity") +
  scale_x_discrete(name = "Shrub Encroachment Level") +
  theme_bw() +
  facet_wrap(vars(Year))+
  theme(legend.position = "top")
DotPlot_SSB_H_20.21


### Site Level Species Richness - All SSB samples summed to the site level
library(vegan)
library(dplyr)
SSB_SiteLvl_20.21_SpRich = SSB_SiteLvl_20.21_m2 #create copy of dataset
str(SSB_SiteLvl_20.21_SpRich)
SSB_SiteLvl_20.21_SpRich$Richness = specnumber(SSB_SiteLvl_20.21_SpRich[,c(6:68)]) # calculate species richness
SSB_SiteLvl_20.21_SpRich$H = diversity(SSB_SiteLvl_20.21_SpRich[,c(6:68)]) # calculate Shannon-Wiener H

library(Rmisc)
SSB_SiteLvl_SpRich_Summary = summarySE(SSB_SiteLvl_20.21_SpRich, "Richness", groupvars = c("Year", "Treatment", "Shrub_Encroach"))
SSB_SiteLvl_SpRich_Summary

SSB_SiteLvl_H_Summary = summarySE(SSB_SiteLvl_20.21_SpRich, "H", groupvars = c("Year", "Treatment", "Shrub_Encroach"))
SSB_SiteLvl_H_Summary

DotPlot_SSB_SiteLvl_Richness_20.21 = ggplot(SSB_SiteLvl_SpRich_Summary) +
  aes(x = Shrub_Encroach, y = Richness, colour = Treatment) +
  geom_point(shape = "circle", 
             size = 6, position = position_dodge(.9)) +
  geom_errorbar(aes(ymin=Richness-se, ymax=Richness+se), width=0.25, position=position_dodge(.9))+
  scale_color_brewer(palette = "Dark2", direction = 1) +
  scale_y_continuous(name = "Seed Bank Species Richness") +
  scale_x_discrete(name = "Shrub Encroachment Level") +
  theme_bw() +
  facet_wrap(vars(Year))+
  theme(legend.position = "top")
ggsave("3_output/results/DotPlot_SSB-Richness_SiteLvl_ShrubEncroach_Treatment_H_h8_w12_300dpi.jpeg",
       plot = DotPlot_SSB_SiteLvl_Richness_20.21, dpi = 300, height = 8 , width = 12, units = "in" )


Richness_SiteLvl_Composite_Fig = plot_grid(DotPlot_Veg_Richness_20.21, DotPlot_SSB_SiteLvl_Richness_20.21,
                                   align="h", ncol=2, nrow=1, hjust = 0.04)
Richness_SiteLvl_Composite_Fig
ggsave("3_output/results/Composite_DotPlot_AbvVegSSB-Richness_SiteLvl_ShrubEncroach_Microsite_H_h8_w12_300dpi.jpeg",
       plot = Richness_SiteLvl_Composite_Fig, dpi = 300, height = 8 , width = 12, units = "in" )

DotPlot_SSB_SiteLvl_H_20.21 = ggplot(SSB_SiteLvl_H_Summary) +
  aes(x = Shrub_Encroach, y = H, colour = Treatment) +
  geom_point(shape = "circle", 
             size = 6, position = position_dodge(.9)) +
  geom_errorbar(aes(ymin=H-se, ymax=H+se), width=0.25, position=position_dodge(.9))+
  scale_color_brewer(palette = "Dark2", direction = 1) +
  scale_y_continuous(name = "Seed Bank Species Diversity") +
  scale_x_discrete(name = "Shrub Encroachment Level") +
  theme_bw() +
  facet_wrap(vars(Year))+
  theme(legend.position = "top")
DotPlot_SSB_SiteLvl_H_20.21

ggsave("3_output/results/DotPlot_SSB-Diversity_SiteLvl_ShrubEncroach_Treatment_H_h8_w12_300dpi.jpeg",
       plot = DotPlot_SSB_SiteLvl_H_20.21, dpi = 300, height = 8 , width = 12, units = "in" )
