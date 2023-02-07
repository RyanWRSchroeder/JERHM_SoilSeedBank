#### Data Wrangling - JERHM SSB - SRM 2023 Project ####

# Shrub Island Soil Seed Bank data (2020 & 2021) from control plots regressed against the paired shrub island size # 

#### Packages ####
library(dplyr)
library(tidyr)
library(esquisse)
library(ggplot2)

#### Shrub island data to only be 2020 and 2021

# Filter to make only a Control Shrub dataset 
SHRUB_20.21.Control = SHRUB_20.21.22.Control[SHRUB_20.21.22.Control$Year != "2022",]
summary(SHRUB_20.21.Control)

SSB_20.21_m2.SHRUB.Control.Join = full_join(SSB_20.21_m2.SHRUB.Control, SHRUB_20.21.Control, by = c("Plot_ID", "Year"))
SSB_20.21_m2.SHRUB.Control.Join = SSB_20.21_m2.SHRUB.Control.Join[,-c(9:71)]

esquisser()
