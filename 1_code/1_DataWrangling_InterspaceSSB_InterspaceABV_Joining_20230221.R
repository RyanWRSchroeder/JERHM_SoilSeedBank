#### Data Wrangling - JERHM SSB - SRM 2023 Project ####

#### Packages ####
library(dplyr)
library(tidyr)

#### Interspace, Control Data - Joining Soil Seed Bank Functional Group and Aboveground Veg Funcitonal Group #### 
INT_20.21.22_CoverPercent.Control.Wide = INT_20.21.22_CoverPercent.Control.Wide %>% 
  select(!(Block_Num:Transect_Num))

Interspace_Control_SSB.Abv = left_join(SSB_20.21_m2.INT.Control.Wide, INT_20.21.22_CoverPercent.Control.Wide, by = "Plot_ID")

library(esquisse)
esquisser()

library(PerformanceAnalytics)

chart.Correlation(Interspace_Control_SSB.Abv[,c(8,9,12:16,46:55)], pch = "+")
#chart.Correlation(Interspace_Control_SSB.Abv[,17:], pch = "+")
