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
Clean$people_pred <- NA


# Save Clean to a rds dataset ---------------------------------------------

saveRDS(Clean, "_SharedFolder_article_pot-growth/data/warehouse/step1_people_predict/ces21.rds")
