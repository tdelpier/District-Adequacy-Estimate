


# Setup
library(here)
i_am("flag_project_root.R")
fiscal.year <- 2021
base.foundation <- 10413

do <- function(r.file){
  source(here("Adequacy Estimates/District Adequacy Estimate/2021_Data/",
              paste0(r.file)))
}

do("AdqEst_Org_Setup.R")


# Import Data
do("AdqEst_Import_Enrollment.R")


do("AdqEst_Import_FID.R")
do("AdqEst_Import_CY.R")


# Data Work
do("AdqEst_DataWork_Add-Cost.R")
do("AdqEst_DataWork_ISD-Rev.R")
do("AdqEst_DataWork_Adq-Est.R")
do("AdqEst_DataWork_PK.R")



# total actual revenue should be around $21.4 billion

Adq_Data %>% 
  summarise(actual.rev = sum(actual.rev))




