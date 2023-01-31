#### Data Onboarding - JERHM SSB - SRM 2023 Project ####

# Loading data for the Society for Range Management (2023) and BIOS 595 class project
### Data Needs ###
### Soil seed bank counts -> 2020 and 2021
### Aboveground Vegetation (Interspace and Shrub Island) --> 2020, 2021, 2022

#### Packages ####
library(dplyr)

#### Data Loading #### 
### Soil Seed Bank 
SSB_2020 = read.csv("0_data/JERHM_GreenhouseSoilSeedBank/JERHM_2020_SeedBankSpeciesMatrix_20230127.csv") # Soil Seed Bank 2020 matrix (count data)
SSB_2021 = read.csv("0_data/JERHM_GreenhouseSoilSeedBank/JERHM_2021_SeedBankSpeciesMatrix_20230127.csv") # Soil Seed Bank 2021 matrix (count data)
SSB_2021_Factors = read.csv("0_data/JERHM_GreenhouseSoilSeedBank/JERHM_2021_SeedBankSampleFactors_20230127.csv") # Soil Seed Bank 2021 factors to join

str(SSB_2020)
str(SSB_2021)

### Aboveground Veg

INT_20.21.22 = read.csv("0_data/JERHM_Vegetation/JERHM_Interspace_2020_2021_2022_20230127.csv") # Interspace Veg data (cover classes)
SHRUB_20.21.22 = read.csv("0_data/JERHM_Vegetation/JERHM_ShrubIsland_2020_2021_2022_20230127.csv") # Shrub Island data (perimeter & height) 

str(INT_20.21.22)
str(SHRUB_20.21.22)

### Soil Nutrient

NUTR_2020 = read.csv("0_data/JERHM_SoilNutrient/JERHM_2020_SoilNutrientData_Master_20230127.csv") # Soil nutrient 2020 (nutrient concentrations)
NUTR_2021 = read.csv("0_data/JERHM_SoilNutrient/JERHM_2021_SoilNutrientData_Master_20230127.csv") # Soil nutrient 2021 (nutrient concentrations)

str(NUTR_2020)
str(NUTR_2021)

