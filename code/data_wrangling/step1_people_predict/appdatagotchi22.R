# Load packages ----------------------------------------------------------------
library(tidyverse)
library(sondr)

# Load raw data -----------------------------------------------------------
Raw <- readRDS("_SharedFolder_article_pot-growth/data/lake/datagotchi_2022_app/data-hub-clean-2022-10-27_clean.rds")

# Create empty clean dataframe --------------------------------------------
Clean <- data.frame(id = 1:nrow(Raw), # id of the respondent
                    source_id = "appdatagotchi22", # id of the survey
                    year = 2022, # year of the survey
                    level = "prov_qc") # fed_can or prov_qc

# Clean variables ---------------------------------------------------------

## riding ------------------------------------------------------------------



## indicator of political sophistication ------------------------------------------------------------------
Clean$pol_sophis[Raw$educBHS == 1] <- 0
Clean$pol_sophis[Raw$educCollege == 1] <- 0.5
Clean$pol_sophis[Raw$educUniv == 1] <- 1
table(Clean$pol_sophis)


## riding prediction ---------------------------------------------------------------------

# People predict CAQ
table(Raw$people_pred_CAQ)
Clean$people_pred_CAQ <- NA
Clean$people_pred_CAQ[Raw$people_pred_CAQ == 0 ]<- 0
Clean$people_pred_CAQ[Raw$people_pred_CAQ == 1 ]<- 1
table(Clean$people_pred_CAQ)

# People predict PQ
table(Raw$people_pred_PQ)
Clean$people_pred_PQ <- NA
Clean$people_pred_PQ[Raw$people_pred_PQ == 0 ]<- 0
Clean$people_pred_PQ[Raw$people_pred_PQ == 1 ]<- 1
table(Clean$people_pred_PQ)

# People predict PLQ
table(Raw$people_pred_PLQ)
Clean$people_pred_PLQ <- NA
Clean$people_pred_PLQ[Raw$people_pred_PLQ == 0 ]<- 0
Clean$people_pred_PLQ[Raw$people_pred_PLQ == 1 ]<- 1
table(Clean$people_pred_PLQ)

# People predict QS
table(Raw$people_pred_QS)
Clean$people_pred_QS <- NA
Clean$people_pred_QS[Raw$people_pred_QS == 0 ]<- 0
Clean$people_pred_QS[Raw$people_pred_QS == 1 ]<- 1
table(Clean$people_pred_QS)

# People predict PCQ
table(Raw$people_pred_PCQ)
Clean$people_pred_PCQ <- NA
Clean$people_pred_PCQ[Raw$people_pred_PCQ == 0 ]<- 0
Clean$people_pred_PCQ[Raw$people_pred_PCQ == 1 ]<- 1
table(Clean$people_pred_PCQ)

# Save Clean to a rds dataset ---------------------------------------------

saveRDS(Clean, "_SharedFolder_article_pot-growth/data/warehouse/step1_people_predict/")
