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
INT_20.21.22$Grazed_Ungrazed = as.character(INT_20.21.22$Grazed_Ungrazed)

# Drop unneeded variables: Date, Initials_1, Initials_2, Azimuth, Random_1 through Random_4 
INT_20.21.22 = INT_20.21.22 %>% 
  select(-Date, -Initials_1, -Initials_2, -Azimuth, -Random_1, -Random_2, - Random_3, -Random_4)
str(INT_20.21.22)

# Replace "N/A" values to be na in R
INT_20.21.22 = INT_20.21.22 %>% 
  mutate(across(where(is.character), ~na_if(., "N/A"))) # replaces "N/A" with true na missing values (as data were not recorded)
str(INT_20.21.22)

INT_20.21.22$Grazed_Ungrazed = as.factor(INT_20.21.22$Grazed_Ungrazed) # coerce to be a factor

# Replace na values in Annual_Forb_Seelding_count to be 0s since since observed
INT_20.21.22 = INT_20.21.22 %>% mutate(An_Forb_Seedling_count = ifelse(is.na(An_Forb_Seedling_count), 0, An_Forb_Seedling_count)) # replace na values with 0

#Replace Cover classes (Bailey and Poulton, 1968) in the cover class columns with the mean value of the cover class and coerce to numeric

class_string = c('0'='0', 'T'='0.5', '1'='2.5', '2'='15', '3'='37.5', '4'='62.5', '5'='85', '6'='97.5') # numbers that correspond to each class number
class_string

INT_20.21.22_CoverPercent = INT_20.21.22
str(INT_20.21.22_CoverPercent)

INT_20.21.22_CoverPercent['Bare_Class'][INT_20.21.22_CoverPercent['Bare_Class'] == '6'] = 97.5


library(stringr)
# INT_20.21.22$Bare_Class = str_replace_all(INT_20.21.22$Bare_Class, class_string) # replace class values

