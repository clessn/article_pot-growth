# Load packages ----------------------------------------------------------------
library(tidyverse)
library(sondr)

# Load raw data -----------------------------------------------------------
Raw <- sondr::read_any_csv("_SharedFolder_article_pot-growth/data/lake/datagotchi_2022_pilote1/Pilote1-donnees-geo.csv")

# Create empty clean dataframe --------------------------------------------
Clean <- data.frame(id = 1:nrow(Raw), # id of the respondent
                    source_id = "pilote1datagotchi22", # id of the survey
                    year = 2022, # year of the survey
                    level = "prov_qc") # fed_can or prov_qc
# Clean variables ---------------------------------------------------------

## gender ------------------------------------------------------------------
table(Raw$male)
Clean$male <- NA
Clean$male[as.numeric(Raw$male) == 1] <- 1
Clean$male[as.numeric(Raw$male) == 0] <- 0
table(Clean$male)

table(Raw$female)
Clean$female <- NA
Clean$female[as.numeric(Raw$female) == 1] <- 1
Clean$female[as.numeric(Raw$female) == 0] <- 0
table(Clean$female)

## age ------------------------------------------------------------------
table(Raw$age)

## language ------------------------------------------------------------------

## riding ------------------------------------------------------------------

## RCI ---------------------------------------------------------------------


# Save Clean to a rds dataset ---------------------------------------------

saveRDS(Clean, "_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/separated/omnibus_january.rds")
