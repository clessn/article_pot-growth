# Load packages ----------------------------------------------------------------
library(tidyverse)
library(sondr)

# Load raw data -----------------------------------------------------------
Raw <- sondr::read_any_csv("_SharedFolder_article_pot-growth/data/lake/ces2021/ces2021.csv")

# Create empty clean dataframe --------------------------------------------
Clean <- data.frame(id = 1:nrow(Raw), # id of the respondent
                    source_id = "ces21", # id of the survey
                    year = 2021, # year of the survey
                    level = "fed_can") # fed_can or prov_qc

# Clean variables ---------------------------------------------------------

## riding ------------------------------------------------------------------

table(Raw$feduid)
table(Raw$fedname)

Clean$riding_id <- Raw$feduid
Clean$riding_name <- Raw$fedname

Encoding(Clean$riding_name) <- "latin1"

## indicator of political sophistication ------------------------------------------------------------------
#### cps21_interest_gen_1
table(Raw$cps21_interest_gen_1)
Clean$pol_sophis <- NA
Clean$pol_sophis<- as.numeric(Raw$cps21_interest_gen_1)
Clean$pol_sophis[as.numeric(Raw$cps21_interest_gen_1) == -99] <- NA
Clean$pol_sophis <- Clean$pol_sophis / 10
table(Clean$pol_sophis)


## riding prediction ---------------------------------------------------------------------

## People predict Libéral
table(Raw$cps21_win_local_1)
Clean$people_pred_PLC <- NA
Clean$people_pred_PLC <- as.numeric(Raw$cps21_win_local_1)
Clean$people_pred_PLC[as.numeric(Raw$cps21_win_local_1) == -99] <- NA
Clean$people_pred_PLC <- Clean$people_pred_PLC / 100
table(Clean$people_pred_PLC)

## People predict conservateur
table(Raw$cps21_win_local_2)
Clean$people_pred_PCC <- NA
Clean$people_pred_PCC <- as.numeric(Raw$cps21_win_local_2)
Clean$people_pred_PCC[as.numeric(Raw$cps21_win_local_2) == -99] <- NA
Clean$people_pred_PCC <- Clean$people_pred_PCC / 100
table(Clean$people_pred_PCC)

## People predict NPD
table(Raw$cps21_win_local_3)
Clean$people_pred_NPD <- NA
Clean$people_pred_NPD <- as.numeric(Raw$cps21_win_local_3)
Clean$people_pred_NPD[as.numeric(Raw$cps21_win_local_3) == -99] <- NA
Clean$people_pred_NPD <- Clean$people_pred_NPD / 100
table(Clean$people_pred_NPD)

## People predict Bloc québécois
table(Raw$cps21_win_local_4)
Clean$people_pred_BQ <- NA
Clean$people_pred_BQ <- as.numeric(Raw$cps21_win_local_4)
Clean$people_pred_BQ[as.numeric(Raw$cps21_win_local_4) == -99] <- NA
Clean$people_pred_BQ <- Clean$people_pred_BQ / 100
table(Clean$people_pred_BQ)

## People predict Parti vert
table(Raw$cps21_win_local_5)
Clean$people_pred_PVC <- NA
Clean$people_pred_PVC <- as.numeric(Raw$cps21_win_local_5)
Clean$people_pred_PVC[as.numeric(Raw$cps21_win_local_5) == -99] <- NA
Clean$people_pred_PVC <- Clean$people_pred_PVC / 100
table(Clean$people_pred_PVC)

# Save Clean to a rds dataset ---------------------------------------------

saveRDS(Clean, "_SharedFolder_article_pot-growth/data/warehouse/step1_people_predict/ces21.rds")
