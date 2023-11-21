# Load packages ----------------------------------------------------------------
library(tidyverse)
library(sondr)

# Load raw data -----------------------------------------------------------
Raw <- sondr::read_any_csv("_SharedFolder_article_pot-growth/data/lake/datagotchi_2022_pilote2/datagotchi_pilot2_2022.csv")

# Create empty clean dataframe --------------------------------------------
Clean <- data.frame(id = 1:nrow(Raw), # id of the respondent
                    source_id = "pilote2datagotchi22", # id of the survey
                    year = 2022, # year of the survey
                    level = "prov_qc") # fed_can or prov_qc

# Clean variables ---------------------------------------------------------

## gender ------------------------------------------------------------------
table(Raw$gender)
Clean$male <- NA
Clean$male[as.numeric(Raw$gender) %in% c(1,3)] <- 1
Clean$male[as.numeric(Raw$gender) %in% c(2,4,5,6,7,8)] <- 0
table(Clean$male)

table(Raw$gender)
Clean$female <- NA
Clean$female[as.numeric(Raw$gender) %in% c(2,4)] <- 1
Clean$female[as.numeric(Raw$gender) %in% c(1,3,5,6,7,8)] <- 0
table(Clean$female)

## age ------------------------------------------------------------------
table(Raw$age)

## language ------------------------------------------------------------------

## riding ------------------------------------------------------------------

## RCI ---------------------------------------------------------------------


# Save Clean to a rds dataset ---------------------------------------------

saveRDS(Clean, "_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/separated/omnibus_january.rds")
