#### Data Wrangling - JERHM SSB - SRM 2023 Project ####

#### Packages ####
library(dplyr)
library(tidyr)

#### Aboveground Vegetation Data Wrangling #### 
# Interspace and Shrub Island vegetation data to be wrangled

#### Interspace Data #### 
# Data collected from 2020, 2021, and 2022
str(INT_20.21.22)
# coerce main plot groups to factors
INT_20.21.22$Plot_ID = as.factor(INT_20.21.22$Plot_ID)
INT_20.21.22$Block_Num = as.factor(INT_20.21.22$Block_Num)
INT_20.21.22$Treatment = as.factor(INT_20.21.22$Treatment)
INT_20.21.22$Interspace_Num = as.factor(INT_20.21.22$Interspace_Num)
INT_20.21.22$Transect_Num = as.factor(INT_20.21.22$Transect_Num)
INT_20.21.22$Year = as.factor(INT_20.21.22$Year)

# Drop unneeded variables: Date, Initials_1, Initials_2, Azimuth, Random_1 through Random_4, Notes, 
