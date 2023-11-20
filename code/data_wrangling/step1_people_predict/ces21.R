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
Clean$people_pred_liberal <- NA
Clean$people_pred_liberal <- as.numeric(Raw$cps21_win_local_1)
Clean$people_pred_liberal[as.numeric(Raw$cps21_win_local_1) == -99] <- NA
Clean$people_pred_liberal <- Clean$people_pred_liberal / 100
table(Clean$people_pred_liberal)

## People predict conservateur
table(Raw$cps21_win_local_2)
Clean$people_pred_conservateur <- NA
Clean$people_pred_conservateur <- as.numeric(Raw$cps21_win_local_2)
Clean$people_pred_conservateur[as.numeric(Raw$cps21_win_local_2) == -99] <- NA
Clean$people_pred_conservateur <- Clean$people_pred_conservateur / 100
table(Clean$people_pred_conservateur)

## People predict NPD
table(Raw$cps21_win_local_3)
Clean$people_pred_npd <- NA
Clean$people_pred_npd <- as.numeric(Raw$cps21_win_local_3)
Clean$people_pred_npd[as.numeric(Raw$cps21_win_local_3) == -99] <- NA
Clean$people_pred_npd <- Clean$people_pred_npd / 100
table(Clean$people_pred_npd)

## People predict Bloc québécois
table(Raw$cps21_win_local_4)
Clean$people_pred_blocqueb <- NA
Clean$people_pred_blocqueb <- as.numeric(Raw$cps21_win_local_4)
Clean$people_pred_blocqueb[as.numeric(Raw$cps21_win_local_4) == -99] <- NA
Clean$people_pred_blocqueb <- Clean$people_pred_blocqueb / 100
table(Clean$people_pred_blocqueb)

## People predict Parti vert
table(Raw$cps21_win_local_5)
Clean$people_pred_partivert <- NA
Clean$people_pred_partivert <- as.numeric(Raw$cps21_win_local_5)
Clean$people_pred_partivert[as.numeric(Raw$cps21_win_local_5) == -99] <- NA
Clean$people_pred_partivert <- Clean$people_pred_partivert / 100
table(Clean$people_pred_partivert)

# Save Clean to a rds dataset ---------------------------------------------

saveRDS(Clean, "_SharedFolder_article_pot-growth/data/warehouse/step1_people_predict/ces21.rds")
