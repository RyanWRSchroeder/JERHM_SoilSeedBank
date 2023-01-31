#### Data Wrangling - JERHM SSB - SRM 2023 Project ####

#### Packages ####
library(dplyr)
library(tidyr)

#### Soil Seed Bank Data Wrangling #### 
## Join Spp Matrix and Factor IDs
SSB_2021 = full_join(SSB_2021_Factors, SSB_2021, by = "Cell_Number_2021") # joins the two datasets and keeps all rows from Factors dataset

SSB_2021_ControlCells = SSB_2021[c(505:555),] # Control cell seedling counts only
SSB_2021 = SSB_2021[c(1:504),] # remove control Cells from Matrix 

str(SSB_2021)
 

SSB_2021[is.na(SSB_2021)] = 0 # Replace NA values with 0
sum(SSB_2021[,c(11:76)]) # 3027 seedlings (including contaminants) and appears to be correct

# Need to pivot the dataset to longform in order to combine the seedling counts from cells that had to be split (Cell IDs ending in "_2")

SSB_2021_long = SSB_2021 %>% 
  pivot_longer(cols = c(11:76), names_to = "Species", values_to = "count") # pivot the species data to be long so that can bring data back together with just the Plot_ID
View(SSB_2021_long)
sum(SSB_2021_long$count) # 3027 --> correct

SSB_2021_wide = SSB_2021_long %>%  
  pivot_wider(id_cols = Plot_ID, names_from = Species, values_from = count, values_fn = sum)
sum(SSB_2021_wide[,c(2:67)]) # 3027 as it should be

SSB_2021_Factors.480 = SSB_2021_Factors[c(1:480),] # Create factors to join to the new Wide 2021 SSB

SSB_2021 = full_join(SSB_2021_Factors.480, SSB_2021_wide, by = "Plot_ID")
sum(SSB_2021[,c(11:76)]) #3027 as it should be

## need to remove Cell_Number_2021 and Cell_ID columns

SSB_2021 = SSB_2021 %>%
  select(-Cell_Number_2021, -Cell_ID) # Drops these two columns from the dataset 

# Now need to join the 2020 and 2021 SSB datasets... but, there are mismatching species between the two datasets. Would be possible to pivot longer, rbind() them and then
# apply Factors to them? 

SSB_2021_long = SSB_2021 %>% 
  pivot_longer(cols = c(9:74), names_to = "Species", values_to = "count") # pivot the species data to be long so that can bring data back together with just the Plot_ID
View(SSB_2021_long)
sum(SSB_2021_long$count) # 3027 --> correct


SSB_2020_long = SSB_2020 %>% 
  pivot_longer(cols= c(9:77), names_to = "Species", values_to = "count")
View(SSB_2020_long) # looks good
sum(SSB_2020_long$count) #2447 --> correct

SSB_20.21_long = rbind(SSB_2020_long, SSB_2021_long) # combine the two datasets
sum(SSB_20.21_long$count) # 5474 --> correct
str(SSB_20.21_long) # 59832 rows (28152 from 2020 + 31680 from 2021, correct)

# pivot back out (attempted to keep all columns)

SSB_20.21_wide = SSB_20.21_long %>% 
  pivot_wider(id_cols = c(Plot_ID, Year, Block_Num, Treatment, Shrub_Interspace, Transect, EcolState, Shrub_Encroach),
              names_from = Species, 
              values_from = count,
              values_fill = 0)
View(SSB_20.21_wide)
str(SSB_20.21_wide)
sum(SSB_20.21_wide[,c(9:98)]) # 5474 seedlings, correct! 

# Species not shared between years added after VUOC (i.e. species found only in 2021) 
# GUSA in 2021 needs to be GUSA2 
sum(SSB_20.21_wide$GUSA2) # 7
sum(SSB_20.21_wide$GUSA) # 15 

SSB_20.21_wide$GUSA2 = SSB_20.21_wide$GUSA2 + SSB_20.21_wide$GUSA
sum(SSB_20.21_wide$GUSA2) # 22 

SSB_20.21_wide = SSB_20.21_wide %>% 
  select(-GUSA)

#### Remove Contaminant Species from Matrix #### 
# Species to be removed: ASTER_CONTAM_ERDI4, CIRSIUM_CONTAM, MONOCOT_CONTAM, MUSTARD_CONTAM, OXCO_CONTAM, PALMATE_CONTAM, RUMEX_CONTAM, SUCCULENT_CONTAM, 
str(SSB_20.21_wide)

SSB_20.21_wide = SSB_20.21_wide %>% 
  select(-ASTER_CONTAM_ERDI4, -CIRSIUM_CONTAM, -MONOCOT_CONTAM, -MUSTARD_CONTAM, -OXCO_CONTAM, -PALMATE_CONTAM, -RUMEX_CONTAM, -SUCCULENT_CONTAM)
sum(SSB_20.21_wide[,c(9:89)]) # 5392 seedlings (82 contaminants)

##### Sum species to total ####
SSB_20.21_wide$Total = rowSums(SSB_20.21_wide[,c(9:89)]) # calculate Total Seed Bank sum for each row 
# 81 unique taxa

#####
write.csv(SSB_20.21_wide, file = "2_incremental/JERHM_2020_2021_SoilSeedBankSpeciesMatrix_count_20230128.csv") # write csv file of the combined Seed Bank Species Matrix

##### Coerce Grouping variables to factors ####

SSB_20.21_wide$Plot_ID = as.factor(SSB_20.21_wide$Plot_ID)
SSB_20.21_wide$Year = as.factor(SSB_20.21_wide$Year)
SSB_20.21_wide$Block_Num = as.factor(SSB_20.21_wide$Block_Num)
SSB_20.21_wide$Treatment = as.factor(SSB_20.21_wide$Treatment)
SSB_20.21_wide$Shrub_Interspace = as.factor(SSB_20.21_wide$Shrub_Interspace)
SSB_20.21_wide$Transect = as.factor(SSB_20.21_wide$Transect)
SSB_20.21_wide$EcolState = as.factor(SSB_20.21_wide$EcolState)
SSB_20.21_wide$Shrub_Encroach = as.factor(SSB_20.21_wide$Shrub_Encroach)
SSB_20.21_wide$Shrub_Encroach = factor(SSB_20.21_wide$Shrub_Encroach,levels = c("Low","Medium","High")) # coerce the factors into Low -> High for graphing purposes

str(SSB_20.21_wide)

#### Lump various species #### 
# Species to lump:
# CHAMA15 (native annual sandmats) = CHAMA15 + CHGL13 + CHMI7 + CHSE6 + CHSE7  
# CYRPT (native annual cryptanthas) = CRAN4 + CRCR3 + CRMI 
# ERAGR (introduced perennial grasses) = ERCU2 + ERLE 
# HOFFM (native perennial Hoffmansseggias) = HOFFM + HOGL2
# KALLS (native annual Kallstromias) = KAHI + KAPA
# PECTI (native annual Pectis) = PEAN + PECTI + PEPA2
# PORTU (native annual Portulacas) = POHA5 + POPI3 + PORTU
# SPORO (native perennial Sporobolus) = SPCO4 + SPCR + SPFL2 + SPNE + SPORO
# 

# Chamasyce (native annuals)
SSB_20.21_wide$CHAMA15 = SSB_20.21_wide$CHAMA15 + SSB_20.21_wide$CHGL13 + SSB_20.21_wide$CHMI7 + 
  SSB_20.21_wide$CHSE6 + SSB_20.21_wide$CHSE7
sum(SSB_20.21_wide$CHAMA15) # 22 
SSB_20.21_wide = SSB_20.21_wide %>%  # Drop the other taxa
  select(-CHGL13, -CHMI7, -CHSE6, -CHSE7)

# Cryptantha (native annuals)
SSB_20.21_wide$CRYPT = SSB_20.21_wide$CRAN4 + SSB_20.21_wide$CRCR3 + SSB_20.21_wide$CRMI + SSB_20.21_wide$CRYPT
sum(SSB_20.21_wide$CRYPT) # 417
SSB_20.21_wide = SSB_20.21_wide %>% 
  select(-CRAN4, -CRCR3, -CRMI)

# Eragrostis (introduced perennial grasses)
SSB_20.21_wide$ERAGR = SSB_20.21_wide$ERCU2 + SSB_20.21_wide$ERLE
sum(SSB_20.21_wide$ERAGR) # 236 
SSB_20.21_wide = SSB_20.21_wide %>% 
  select(-ERCU2, -ERLE)

# HOFFM (native perennial Hoffmansseggia) 
SSB_20.21_wide$HOFFM = SSB_20.21_wide$HOFFM + SSB_20.21_wide$HOGL2
sum(SSB_20.21_wide$HOFFM)
SSB_20.21_wide = SSB_20.21_wide %>% 
  select(-HOGL2)

# KALLS (native annual Kallstromias) 
SSB_20.21_wide$KALLS = SSB_20.21_wide$KAHI + SSB_20.21_wide$KAPA
sum(SSB_20.21_wide$KALLS)
SSB_20.21_wide = SSB_20.21_wide %>% 
  select(-KAHI, -KAPA)

# PECTI (native annual Pectis) 
SSB_20.21_wide$PECTI = SSB_20.21_wide$PEAN + SSB_20.21_wide$PECTI + SSB_20.21_wide$PEPA2
sum(SSB_20.21_wide$PECTI)
SSB_20.21_wide = SSB_20.21_wide %>% 
  select(-PEAN, -PEPA2)

# PORTU (native annual Portulaca) 
SSB_20.21_wide$PORTU = SSB_20.21_wide$POHA5 + SSB_20.21_wide$POPI3 + SSB_20.21_wide$PORTU
sum(SSB_20.21_wide$PORTU) # 52
SSB_20.21_wide = SSB_20.21_wide %>% 
  select(-POHA5, -POPI3)

# SPORO (native perennial Sporobolus)
SSB_20.21_wide$SPORO = SSB_20.21_wide$SPCO4 + SSB_20.21_wide$SPCR + SSB_20.21_wide$SPFL2 + 
  SSB_20.21_wide$SPNE + SSB_20.21_wide$SPORO
sum(SSB_20.21_wide$SPORO) # 829
SSB_20.21_wide = SSB_20.21_wide %>% 
  select(-SPCO4, -SPCR, -SPFL2, -SPNE)

#### Sum Seedlings by functional groups ####

SSB_20.21_wide$Shrub = rowSums(SSB_20.21_wide[,c("ATCA2", "PRGL2")]) # calculate Shrub fnc group sums
SSB_20.21_wide$Introduced_Annual_Forb = rowSums(SSB_20.21_wide[,c("AMAL", "MOCE")])
SSB_20.21_wide$Introduced_Annual_Grass = rowSums(SSB_20.21_wide[,c("ECCR")])
SSB_20.21_wide$Introduced_Perennial_Grass = rowSums(SSB_20.21_wide[,c("ERAGR")])  
SSB_20.21_wide$Native_Annual_Forb = rowSums(SSB_20.21_wide[,c("CHAMA15", "CRYPT","KALLS", "PECTI", "PORTU",
                                                              "AMPA", "APRA", "ASNU4", "BOWR", "CHIN2",
                                                              "DEPI", "ERAB2", "HEPE", "HOHU", "HYWI",
                                                              "IPCO2","LACO13", "LACTU", "LIAU4", "MOVE",
                                                              "NAHI", "PASP", "PLPA2", "TILA2", "VEEN")])
SSB_20.21_wide$Native_Annual_Grass = rowSums(SSB_20.21_wide[,c("ARAD", "BOAR", "BOBA2", "LEPAB", "MUSQ",
                                                               "PAHI5", "URAR", "VUOC")])
SSB_20.21_wide$Native_Perennial_Forb = rowSums(SSB_20.21_wide[,c("HOFFM", "BAAB", "BAMU", "CHAL11", "CHSO",
                                                                 "CIOC2", "DALA3", "DIWI2", "ERDI4", "GUSA2",
                                                                 "MAPI", "RUME2", "SPAN3", "SPIN2")])
SSB_20.21_wide$Native_Perennial_Grass = rowSums(SSB_20.21_wide[,c("SPORO", "ARPU9", "BOER4", "CYRE14", "DAPU7",
                                                                  "Fuzzy_Bluegrass", "MUPO2")])

##### Reorder Dataframe ordering ####
str(SSB_20.21_wide)
order = c("Plot_ID", "Year", "Block_Num", "Treatment", "Shrub_Interspace", "Transect", "EcolState", "Shrub_Encroach",
          "AMAL", "AMPA", "APRA", "ARAD", "ARPU9", "ASNU4", "ATCA2", "BAAB", "BAMU", "BOAR", "BOBA2", "BOER4",
          "BOWR", "CHAL11", "CHAMA15", "CHIN2", "CHSO", "CIOC2", "CRYPT", "CYRE14", "DALA3", "DAPU7", "DEPI", "DIWI2", "ECCR",
          "ERAB2", "ERAGR", "ERDI4", "Fuzzy_Bluegrass", "GUSA2", "HEPE", "HOFFM", "HOHU", "HYWI", "IPCO2", "KALLS", "LACO13", "LACTU", "LEPAB", "LIAU4",
          "MAPI", "MOCE", "MOVE", "MUPO2", "MUSQ", "NAHI","PAHI5", "PASP", "PECTI", "PLPA2", "PORTU", "PRGL2", "RUME2", "SPAN3", 
          "SPIN2", "SPORO", "TILA2", "UNK", "UNK_DICOT", "UNK_MONO", "URAR", "VEEN", "VUOC", "Total", "Shrub", 
          "Introduced_Annual_Forb", "Introduced_Annual_Grass", "Introduced_Perennial_Grass", "Native_Annual_Forb",
          "Native_Annual_Grass", "Native_Perennial_Forb", "Native_Perennial_Grass")
SSB_20.21_wide = SSB_20.21_wide[, order] # reorders dataframe 

write.csv(SSB_20.21_wide, file = "2_incremental/JERHM_2020_2021_SoilSeedBankSpeciesMatrix_lumped_count_20230131.csv")

## can run esquisser() and look at data. Looks good! 

##### Transform seedling count data to density (seeds per m^2)

SamplingArea = 0.00353 # area in m^2 sampled for each soil seed bank sample
# Create a function here

#apply(SSB_20.21_wide[, c(9:80)], 2, fun)

#SSB_20.21_wide[,c(9:80)]/SamplingArea
