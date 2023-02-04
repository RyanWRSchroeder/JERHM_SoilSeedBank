#### Data Wrangling - JERHM SSB - SRM 2023 Project ####

#### Packages ####
library(dplyr)
library(tidyr)

#### Aboveground Vegetation Data Wrangling #### 
# Shrub Island vegetation data to be wrangled

#### Shrub Island Data #### 
# Data collected from 2020, 2021, and 2022
str(SHRUB_20.21.22)
# coerce main plot groups to factors
SHRUB_20.21.22$Plot_ID = as.factor(SHRUB_20.21.22$Plot_ID)
SHRUB_20.21.22$Block_Num = as.factor(SHRUB_20.21.22$Block_Num)
SHRUB_20.21.22$Treatment = as.factor(SHRUB_20.21.22$Treatment)
SHRUB_20.21.22$Shrub_Num = as.factor(SHRUB_20.21.22$Shrub_Num)
SHRUB_20.21.22$Transect_Num = as.factor(SHRUB_20.21.22$Transect_Num)
SHRUB_20.21.22$Year = as.factor(SHRUB_20.21.22$Year)
str(SHRUB_20.21.22)

# Drop unneeded variables: Date, Initials_1, Initials_2, Random_1 through Random_20, Grazed_2022, Transect_Dist_m, Notes, Estimated_CanopY...  
SHRUB_20.21.22 = SHRUB_20.21.22 %>% 
  select(-Date, -Initials_1, -Initials_2, 
         -Random_1, -Random_2, -Random_3, -Random_4, -Random_5,
         -Random_6, -Random_7, -Random_8, -Random_9, -Random_10,
         -Random_11, -Random_12, -Random_13, -Random_14, -Random_15,
         -Random_16, -Random_17, -Random_18, -Random_19, -Random_20, 
         -Grazed_2022, -Transect_Dist_m, -Notes, -Estimated_Canopy_Regrowth_Percent)
str(SHRUB_20.21.22)

# Replace "N/A" values to be na in R
SHRUB_20.21.22 = SHRUB_20.21.22 %>% 
  mutate(across(where(is.character), ~na_if(., "N/A"))) # replaces "N/A" with true na missing values (as data were not recorded)
str(SHRUB_20.21.22)



# Replace height classes with mean height values
SHRUB_20.21.22['Height_Class'][SHRUB_20.21.22['Height_Class'] == '>2.5'] = 2.75
SHRUB_20.21.22['Height_Class'][SHRUB_20.21.22['Height_Class'] == '0.5-1'] = 0.75
SHRUB_20.21.22['Height_Class'][SHRUB_20.21.22['Height_Class'] == '0.5-1.0'] = 0.75
SHRUB_20.21.22['Height_Class'][SHRUB_20.21.22['Height_Class'] == '1-1.5'] = 1.25 
SHRUB_20.21.22['Height_Class'][SHRUB_20.21.22['Height_Class'] == '1.0-1.5'] = 1.25
SHRUB_20.21.22['Height_Class'][SHRUB_20.21.22['Height_Class'] == '1.5-2'] = 1.75
SHRUB_20.21.22['Height_Class'][SHRUB_20.21.22['Height_Class'] == '1.5-2.0'] = 1.75  
SHRUB_20.21.22['Height_Class'][SHRUB_20.21.22['Height_Class'] == '2-2.5'] = 2.25
SHRUB_20.21.22['Height_Class'][SHRUB_20.21.22['Height_Class'] == '2.0-2.5'] = 2.25
SHRUB_20.21.22['Height_Class'][SHRUB_20.21.22['Height_Class'] == '2.5-3.0'] = 2.75
SHRUB_20.21.22['Height_Class'][SHRUB_20.21.22['Height_Class'] == '3.0-3.5'] = 2.75 # since Only was supposed to take max of >2.5

SHRUB_20.21.22$Height_Class = as.numeric(SHRUB_20.21.22$Height_Class)
summary(SHRUB_20.21.22$Height_Class)

SHRUB_20.21.22 = SHRUB_20.21.22 %>% 
  rename(Height_m = Height_Class) # Rename column to reflect change to median shrub height (m)
summary(SHRUB_20.21.22$Height_m)

# Drop rows with na values (Blocks 5, 6, 7) for 2020
SHRUB_20.21.22 = SHRUB_20.21.22 %>% 
  drop_na(Height_m)

#Coerce Perimeter to numeric
SHRUB_20.21.22$Perimeter_m = as.numeric(SHRUB_20.21.22$Perimeter_m)

# Create Shrub Island CSV
write.csv(SHRUB_20.21.22, file = "2_incremental/JERHM_2020_2021_2022_ShrubCharacteristics_20230204.csv")
