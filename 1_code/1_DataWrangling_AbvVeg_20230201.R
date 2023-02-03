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
INT_20.21.22_CoverPercent$An_Forb_Seedling_count = as.numeric(INT_20.21.22_CoverPercent$An_Forb_Seedling_count) # coerce to numeric
summary(INT_20.21.22_CoverPercent$An_Forb_Seedling_count)

#Replace Cover classes (Bailey and Poulton, 1968) in the cover class columns with the mean value of the cover class and coerce to numeric

class_string = c('0'='0', 'T'='0.5', '1'='2.5', '2'='15', '3'='37.5', '4'='62.5', '5'='85', '6'='97.5') # numbers that correspond to each class number
class_string

INT_20.21.22_CoverPercent = INT_20.21.22
str(INT_20.21.22_CoverPercent)

# Replace classes with mean cover percentages: Bare Ground
INT_20.21.22_CoverPercent['Bare_Class'][INT_20.21.22_CoverPercent['Bare_Class'] == '6'] = 97.5
INT_20.21.22_CoverPercent['Bare_Class'][INT_20.21.22_CoverPercent['Bare_Class'] == '5'] = 85
INT_20.21.22_CoverPercent['Bare_Class'][INT_20.21.22_CoverPercent['Bare_Class'] == '4'] = 62.5
INT_20.21.22_CoverPercent['Bare_Class'][INT_20.21.22_CoverPercent['Bare_Class'] == '3'] = 37.5
INT_20.21.22_CoverPercent['Bare_Class'][INT_20.21.22_CoverPercent['Bare_Class'] == '2'] = 15
INT_20.21.22_CoverPercent['Bare_Class'][INT_20.21.22_CoverPercent['Bare_Class'] == '1'] = 2.5
INT_20.21.22_CoverPercent['Bare_Class'][INT_20.21.22_CoverPercent['Bare_Class'] == 'T'] = 0.5
INT_20.21.22_CoverPercent['Bare_Class'][INT_20.21.22_CoverPercent['Bare_Class'] == '0'] = 0
INT_20.21.22_CoverPercent$Bare_Class = as.numeric(INT_20.21.22_CoverPercent$Bare_Class)
summary(INT_20.21.22_CoverPercent$Bare_Class)

INT_20.21.22_CoverPercent = INT_20.21.22_CoverPercent %>% 
  rename(Bare_Cover_Pct = Bare_Class) # Rename column to reflect change to cover %
summary(INT_20.21.22_CoverPercent$Bare_Cover_Pct)

# Replace classes with mean cover percentages: Litter
INT_20.21.22_CoverPercent['Litter_Class'][INT_20.21.22_CoverPercent['Litter_Class'] == '6'] = 97.5
INT_20.21.22_CoverPercent['Litter_Class'][INT_20.21.22_CoverPercent['Litter_Class'] == '5'] = 85
INT_20.21.22_CoverPercent['Litter_Class'][INT_20.21.22_CoverPercent['Litter_Class'] == '4'] = 62.5
INT_20.21.22_CoverPercent['Litter_Class'][INT_20.21.22_CoverPercent['Litter_Class'] == '3'] = 37.5
INT_20.21.22_CoverPercent['Litter_Class'][INT_20.21.22_CoverPercent['Litter_Class'] == '2'] = 15
INT_20.21.22_CoverPercent['Litter_Class'][INT_20.21.22_CoverPercent['Litter_Class'] == '1'] = 2.5
INT_20.21.22_CoverPercent['Litter_Class'][INT_20.21.22_CoverPercent['Litter_Class'] == 'T'] = 0.5
INT_20.21.22_CoverPercent['Litter_Class'][INT_20.21.22_CoverPercent['Litter_Class'] == '0'] = 0
INT_20.21.22_CoverPercent$Litter_Class = as.numeric(INT_20.21.22_CoverPercent$Litter_Class)
summary(INT_20.21.22_CoverPercent$Litter_Class)
INT_20.21.22_CoverPercent = INT_20.21.22_CoverPercent %>% 
  rename(Litter_Cover_Pct = Litter_Class)
summary(INT_20.21.22_CoverPercent$Litter_Cover_Pct)

# Replace classes with mean cover percentages: Annual Grass
INT_20.21.22_CoverPercent['An_Grass_Class'][INT_20.21.22_CoverPercent['An_Grass_Class'] == '6'] = 97.5
INT_20.21.22_CoverPercent['An_Grass_Class'][INT_20.21.22_CoverPercent['An_Grass_Class'] == '5'] = 85
INT_20.21.22_CoverPercent['An_Grass_Class'][INT_20.21.22_CoverPercent['An_Grass_Class'] == '4'] = 62.5
INT_20.21.22_CoverPercent['An_Grass_Class'][INT_20.21.22_CoverPercent['An_Grass_Class'] == '3'] = 37.5
INT_20.21.22_CoverPercent['An_Grass_Class'][INT_20.21.22_CoverPercent['An_Grass_Class'] == '2'] = 15
INT_20.21.22_CoverPercent['An_Grass_Class'][INT_20.21.22_CoverPercent['An_Grass_Class'] == '1'] = 2.5
INT_20.21.22_CoverPercent['An_Grass_Class'][INT_20.21.22_CoverPercent['An_Grass_Class'] == 'T'] = 0.5
INT_20.21.22_CoverPercent['An_Grass_Class'][INT_20.21.22_CoverPercent['An_Grass_Class'] == '0'] = 0
INT_20.21.22_CoverPercent$An_Grass_Class = as.numeric(INT_20.21.22_CoverPercent$An_Grass_Class)
summary(INT_20.21.22_CoverPercent$An_Grass_Class)
INT_20.21.22_CoverPercent = INT_20.21.22_CoverPercent %>% 
  rename(Annual_Grass_Cover_Pct = An_Grass_Class)
summary(INT_20.21.22_CoverPercent$Annual_Grass_Cover_Pct)

# Replace classes with mean cover percentages: Perennial Grass
INT_20.21.22_CoverPercent['Per_Grass_Class'][INT_20.21.22_CoverPercent['Per_Grass_Class'] == '6'] = 97.5
INT_20.21.22_CoverPercent['Per_Grass_Class'][INT_20.21.22_CoverPercent['Per_Grass_Class'] == '5'] = 85
INT_20.21.22_CoverPercent['Per_Grass_Class'][INT_20.21.22_CoverPercent['Per_Grass_Class'] == '4'] = 62.5
INT_20.21.22_CoverPercent['Per_Grass_Class'][INT_20.21.22_CoverPercent['Per_Grass_Class'] == '3'] = 37.5
INT_20.21.22_CoverPercent['Per_Grass_Class'][INT_20.21.22_CoverPercent['Per_Grass_Class'] == '2'] = 15
INT_20.21.22_CoverPercent['Per_Grass_Class'][INT_20.21.22_CoverPercent['Per_Grass_Class'] == '1'] = 2.5
INT_20.21.22_CoverPercent['Per_Grass_Class'][INT_20.21.22_CoverPercent['Per_Grass_Class'] == 'T'] = 0.5
INT_20.21.22_CoverPercent['Per_Grass_Class'][INT_20.21.22_CoverPercent['Per_Grass_Class'] == '0'] = 0
INT_20.21.22_CoverPercent$Per_Grass_Class = as.numeric(INT_20.21.22_CoverPercent$Per_Grass_Class)
summary(INT_20.21.22_CoverPercent$Per_Grass_Class)
INT_20.21.22_CoverPercent = INT_20.21.22_CoverPercent %>% 
  rename(Perennial_Grass_Cover_Pct = Per_Grass_Class)
summary(INT_20.21.22_CoverPercent$Perennial_Grass_Cover_Pct)

# Replace classes with mean cover percentages: Annual Forb
INT_20.21.22_CoverPercent['An_Forb_Class'][INT_20.21.22_CoverPercent['An_Forb_Class'] == '6'] = 97.5
INT_20.21.22_CoverPercent['An_Forb_Class'][INT_20.21.22_CoverPercent['An_Forb_Class'] == '5'] = 85
INT_20.21.22_CoverPercent['An_Forb_Class'][INT_20.21.22_CoverPercent['An_Forb_Class'] == '4'] = 62.5
INT_20.21.22_CoverPercent['An_Forb_Class'][INT_20.21.22_CoverPercent['An_Forb_Class'] == '3'] = 37.5
INT_20.21.22_CoverPercent['An_Forb_Class'][INT_20.21.22_CoverPercent['An_Forb_Class'] == '2'] = 15
INT_20.21.22_CoverPercent['An_Forb_Class'][INT_20.21.22_CoverPercent['An_Forb_Class'] == '1'] = 2.5
INT_20.21.22_CoverPercent['An_Forb_Class'][INT_20.21.22_CoverPercent['An_Forb_Class'] == 'T'] = 0.5
INT_20.21.22_CoverPercent['An_Forb_Class'][INT_20.21.22_CoverPercent['An_Forb_Class'] == '0'] = 0
INT_20.21.22_CoverPercent$An_Forb_Class = as.numeric(INT_20.21.22_CoverPercent$An_Forb_Class)
summary(INT_20.21.22_CoverPercent$An_Forb_Class)
INT_20.21.22_CoverPercent = INT_20.21.22_CoverPercent %>% 
  rename(Annual_Forb_Cover_Pct = An_Forb_Class)
summary(INT_20.21.22_CoverPercent$Annual_Forb_Cover_Pct)

# Replace classes with mean cover percentages: Perennial Forb
INT_20.21.22_CoverPercent['Per_Forb_Class'][INT_20.21.22_CoverPercent['Per_Forb_Class'] == '6'] = 97.5
INT_20.21.22_CoverPercent['Per_Forb_Class'][INT_20.21.22_CoverPercent['Per_Forb_Class'] == '5'] = 85
INT_20.21.22_CoverPercent['Per_Forb_Class'][INT_20.21.22_CoverPercent['Per_Forb_Class'] == '4'] = 62.5
INT_20.21.22_CoverPercent['Per_Forb_Class'][INT_20.21.22_CoverPercent['Per_Forb_Class'] == '3'] = 37.5
INT_20.21.22_CoverPercent['Per_Forb_Class'][INT_20.21.22_CoverPercent['Per_Forb_Class'] == '2'] = 15
INT_20.21.22_CoverPercent['Per_Forb_Class'][INT_20.21.22_CoverPercent['Per_Forb_Class'] == '1'] = 2.5
INT_20.21.22_CoverPercent['Per_Forb_Class'][INT_20.21.22_CoverPercent['Per_Forb_Class'] == 'T'] = 0.5
INT_20.21.22_CoverPercent['Per_Forb_Class'][INT_20.21.22_CoverPercent['Per_Forb_Class'] == '0'] = 0
INT_20.21.22_CoverPercent$Per_Forb_Class = as.numeric(INT_20.21.22_CoverPercent$Per_Forb_Class)
summary(INT_20.21.22_CoverPercent$Per_Forb_Class)
INT_20.21.22_CoverPercent = INT_20.21.22_CoverPercent %>% 
  rename(Perennial_Forb_Cover_Pct = Per_Forb_Class)
summary(INT_20.21.22_CoverPercent$Perennial_Forb_Cover_Pct)

# Replace classes with mean cover percentages: Shrub
INT_20.21.22_CoverPercent['Shrub_Class'][INT_20.21.22_CoverPercent['Shrub_Class'] == '6'] = 97.5
INT_20.21.22_CoverPercent['Shrub_Class'][INT_20.21.22_CoverPercent['Shrub_Class'] == '5'] = 85
INT_20.21.22_CoverPercent['Shrub_Class'][INT_20.21.22_CoverPercent['Shrub_Class'] == '4'] = 62.5
INT_20.21.22_CoverPercent['Shrub_Class'][INT_20.21.22_CoverPercent['Shrub_Class'] == '3'] = 37.5
INT_20.21.22_CoverPercent['Shrub_Class'][INT_20.21.22_CoverPercent['Shrub_Class'] == '2'] = 15
INT_20.21.22_CoverPercent['Shrub_Class'][INT_20.21.22_CoverPercent['Shrub_Class'] == '1'] = 2.5
INT_20.21.22_CoverPercent['Shrub_Class'][INT_20.21.22_CoverPercent['Shrub_Class'] == 'T'] = 0.5
INT_20.21.22_CoverPercent['Shrub_Class'][INT_20.21.22_CoverPercent['Shrub_Class'] == '0'] = 0
INT_20.21.22_CoverPercent$Shrub_Class = as.numeric(INT_20.21.22_CoverPercent$Shrub_Class)
summary(INT_20.21.22_CoverPercent$Shrub_Class)
INT_20.21.22_CoverPercent = INT_20.21.22_CoverPercent %>% 
  rename(Shrub_Cover_Pct = Shrub_Class)
summary(INT_20.21.22_CoverPercent$Shrub_Cover_Pct)

# Replace classes with mean cover percentages: Subshrub
INT_20.21.22_CoverPercent['Subshrub_Class'][INT_20.21.22_CoverPercent['Subshrub_Class'] == '6'] = 97.5
INT_20.21.22_CoverPercent['Subshrub_Class'][INT_20.21.22_CoverPercent['Subshrub_Class'] == '5'] = 85
INT_20.21.22_CoverPercent['Subshrub_Class'][INT_20.21.22_CoverPercent['Subshrub_Class'] == '4'] = 62.5
INT_20.21.22_CoverPercent['Subshrub_Class'][INT_20.21.22_CoverPercent['Subshrub_Class'] == '3'] = 37.5
INT_20.21.22_CoverPercent['Subshrub_Class'][INT_20.21.22_CoverPercent['Subshrub_Class'] == '2'] = 15
INT_20.21.22_CoverPercent['Subshrub_Class'][INT_20.21.22_CoverPercent['Subshrub_Class'] == '1'] = 2.5
INT_20.21.22_CoverPercent['Subshrub_Class'][INT_20.21.22_CoverPercent['Subshrub_Class'] == 'T'] = 0.5
INT_20.21.22_CoverPercent['Subshrub_Class'][INT_20.21.22_CoverPercent['Subshrub_Class'] == '0'] = 0
INT_20.21.22_CoverPercent$Subshrub_Class = as.numeric(INT_20.21.22_CoverPercent$Subshrub_Class)
summary(INT_20.21.22_CoverPercent$Subshrub_Class)
INT_20.21.22_CoverPercent = INT_20.21.22_CoverPercent %>% 
  rename(Subshrub_Cover_Pct = Subshrub_Class)
summary(INT_20.21.22_CoverPercent$Subshrub_Cover_Pct)

# Replace classes with mean cover percentages: Gravel
INT_20.21.22_CoverPercent['Gravel_Class'][INT_20.21.22_CoverPercent['Gravel_Class'] == '6'] = 97.5
INT_20.21.22_CoverPercent['Gravel_Class'][INT_20.21.22_CoverPercent['Gravel_Class'] == '5'] = 85
INT_20.21.22_CoverPercent['Gravel_Class'][INT_20.21.22_CoverPercent['Gravel_Class'] == '4'] = 62.5
INT_20.21.22_CoverPercent['Gravel_Class'][INT_20.21.22_CoverPercent['Gravel_Class'] == '3'] = 37.5
INT_20.21.22_CoverPercent['Gravel_Class'][INT_20.21.22_CoverPercent['Gravel_Class'] == '2'] = 15
INT_20.21.22_CoverPercent['Gravel_Class'][INT_20.21.22_CoverPercent['Gravel_Class'] == '1'] = 2.5
INT_20.21.22_CoverPercent['Gravel_Class'][INT_20.21.22_CoverPercent['Gravel_Class'] == 'T'] = 0.5
INT_20.21.22_CoverPercent['Gravel_Class'][INT_20.21.22_CoverPercent['Gravel_Class'] == '0'] = 0
INT_20.21.22_CoverPercent$Gravel_Class = as.numeric(INT_20.21.22_CoverPercent$Gravel_Class)
summary(INT_20.21.22_CoverPercent$Gravel_Class)
INT_20.21.22_CoverPercent = INT_20.21.22_CoverPercent %>% 
  rename(Gravel_Cover_Pct = Gravel_Class)
summary(INT_20.21.22_CoverPercent$Gravel_Cover_Pct)

# Calculate Total Mean Plant Cover Percent
INT_20.21.22_CoverPercent$Total_Plant_Cover_Pct = INT_20.21.22_CoverPercent$Annual_Grass_Cover_Pct +
  INT_20.21.22_CoverPercent$Perennial_Grass_Cover_Pct + INT_20.21.22_CoverPercent$Annual_Forb_Cover_Pct + 
  INT_20.21.22_CoverPercent$Perennial_Forb_Cover_Pct + INT_20.21.22_CoverPercent$Shrub_Cover_Pct +
  INT_20.21.22_CoverPercent$Subshrub_Cover_Pct
summary(INT_20.21.22_CoverPercent$Total_Plant_Cover_Pct)

## Reorder Interspace Dataframe columns
str(INT_20.21.22_CoverPercent)
INT_order = c("Plot_ID", "Year", "Block_Num", "Treatment", "Interspace_Num", "Transect_Num",
              "Grazed_Ungrazed", "Bare_Cover_Pct", "Gravel_Cover_Pct", "Litter_Cover_Pct", "Annual_Grass_Cover_Pct",
              "Perennial_Grass_Cover_Pct", "Annual_Forb_Cover_Pct", "Perennial_Forb_Cover_Pct", "Shrub_Cover_Pct",
              "Subshrub_Cover_Pct", "Total_Plant_Cover_Pct", "An_Forb_Seedling_count", "Notes")
INT_20.21.22_CoverPercent = INT_20.21.22_CoverPercent[, INT_order] # reorders dataframe 
str(INT_20.21.22_CoverPercent)

# Drop rows with na values (Blocks 5, 6, 7) for 2020
INT_20.21.22_CoverPercent = INT_20.21.22_CoverPercent %>% 
  drop_na(Total_Plant_Cover_Pct)

#library(stringr)
# INT_20.21.22$Bare_Class = str_replace_all(INT_20.21.22$Bare_Class, class_string) # replace class values

