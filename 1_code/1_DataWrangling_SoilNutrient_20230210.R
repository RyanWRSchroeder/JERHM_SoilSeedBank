#### Data Wrangling - JERHM SSB - SRM 2023 Project ####

#### Packages ####
library(dplyr)
library(tidyr)

#### Soil Nutrient Data Wrangling #### 
# Soil nutrient data from Ward Labs to be wrangled

#### Nutrient Data #### 
# Data collected from 2020 and 2021

# NUTR_2020 & NUTR_2021
str(NUTR_2020)
str(NUTR_2021)
NUTR_2021 = NUTR_2021 %>%  #rename the Sample_Number to Sample.ID to join the datasets
  rename(Sample.ID = Sample_Number)
# Combine the two datasets (rbind) 
NUTR_20.21 = rbind(NUTR_2020, NUTR_2021) 
str(NUTR_20.21)
NUTR_20.21 = NUTR_20.21 %>% 
  mutate(across(where(is.character), ~na_if(., "N/A"))) # replaces "N/A" with true na missing values (as data were not recorded)
str(NUTR_20.21)
# Coerce Factors
NUTR_20.21$Sample.ID = as.factor(NUTR_20.21$Sample.ID)
NUTR_20.21$Plot_ID = as.factor(NUTR_20.21$Plot_ID)
NUTR_20.21$Year = as.factor(NUTR_20.21$Year)
NUTR_20.21$EcolState = as.factor(NUTR_20.21$EcolState)
NUTR_20.21$Shrub_Encroach = as.factor(NUTR_20.21$Shrub_Encroach)
NUTR_20.21$Shrub_Encroach = factor(NUTR_20.21$Shrub_Encroach,levels = c("Low","Medium","High")) # coerce the factors into Low -> High for graphing purposes
NUTR_20.21$Site = as.factor(NUTR_20.21$Site)
NUTR_20.21$Treatment = as.factor(NUTR_20.21$Treatment)
NUTR_20.21$Transect_Num = as.factor(NUTR_20.21$Transect_Num)
NUTR_20.21$Shrub_Interspace = as.factor(NUTR_20.21$Shrub_Interspace)
str(NUTR_20.21)
NUTR_20.21 = NUTR_20.21 %>% 
  mutate(across(where(is.character), ~na_if(., "N/A"))) # replaces "N/A" with true na missing values (as data were not recorded)
str(NUTR_20.21)
NUTR_20.21$Copper.ppm.Cu = as.numeric(NUTR_20.21$Copper.ppm.Cu)

library(esquisse)
esquisser()

library(Rmisc)
OM_Year_ShrubEncroach_Microsite = summarySE(NUTR_20.21, "Organic.Matter.LOI..", groupvars = c("Year", "Shrub_Encroach", "Shrub_Interspace"))

library(ggplot2)
BarPlot_OM_ShrubEncroach_Microsite_H = OM_Year_ShrubEncroach_Microsite %>% ggplot(aes(y = Organic.Matter.LOI.., x = Shrub_Encroach, fill= Shrub_Interspace))+
  geom_bar(stat = "identity", color = "black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Organic.Matter.LOI..-se, ymax=Organic.Matter.LOI..+se), width=0.25, position=position_dodge(.9)) +
  scale_y_continuous(name = "Soil Organic Material (%)")+
  scale_x_discrete(name = "Shrub Encroachment Level")+
  scale_fill_brewer(palette="Dark2", direction=-1)+
  facet_wrap(~Year, nrow = 1, ncol = 2)+
  labs(title = "Soil Organic Material (2020 & 2021)\n Averaged by Shrub Encroachment Level", fill = "Microsite")+
  theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 14),
        axis.text.y=element_text(color="black", size = 12),
        axis.title.x = element_text(color="black", size=14, margin = margin(10,0,0,0)),
        axis.title.y = element_text(color="black", size=14, margin = margin(0,5,0,0)),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = c(0.9,0.85),
        strip.background =element_rect(fill=NA))
BarPlot_OM_ShrubEncroach_Microsite_H

ggsave("3_output/results/BarPlot_OM_Control_Year_ShrubEncroach_h6_w8_300dpi.jpeg",
       plot = BarPlot_OM_ShrubEncroach_Microsite_H, dpi = 300, height = 6, width = 8, units = "in" )
