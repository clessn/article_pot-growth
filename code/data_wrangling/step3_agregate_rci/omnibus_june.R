# Load packages ----------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(sondr)

# Load raw data -----------------------------------------------------------
Raw <- haven::read_sav("_SharedFolder_article_pot-growth/data/lake/omnibus/june/june.Sav")

# Create empty clean dataframe --------------------------------------------
Clean <- data.frame(id = 1:nrow(Raw), # id of the respondent
                    source_id = "omnibus_june", # id of the survey
                    year = 2022, # year of the survey
                    level = "prov_qc") # fed_can or prov_qc

# Clean variables ---------------------------------------------------------

## gender ------------------------------------------------------------------
table(Raw$SEXE)
Clean$male <- NA
Clean$male[as.numeric(Raw$SEXE) == 1] <- 1
Clean$male[as.numeric(Raw$SEXE) == 2] <- 0
table(Clean$male)

table(Raw$SEXE)
Clean$female <- NA
Clean$female[as.numeric(Raw$SEXE) == 2] <- 1
Clean$female[as.numeric(Raw$SEXE) == 1] <- 0
table(Clean$female)

## age ------------------------------------------------------------------

Clean$age <- case_when(
  Raw$AGE %in% c(2, 3) ~ "34m", # if AGE est l'un de 2 ou 3, mettre 34m
  Raw$AGE %in% c(4, 5) ~ "3554", # if AGE est l'un de 4 ou 5, mettre 3554
  Raw$AGE %in% c(6, 7, 8) ~ "55p" # if AGE est l'un de 6, 7 ou 8, mettre 55p
)
table(Clean$age)

# factorize (donc mettre en catégories)
Clean$age <- factor(Clean$age, levels = c("34m", "3554", "55p")) # levels permet d'ordonner la variable catégorielle


## language ------------------------------------------------------------------
table(Raw$LANGUE)
Clean$anglais <- NA
Clean$anglais[as.numeric(Raw$LANGUE) == 2] <- 1
Clean$anglais[as.numeric(Raw$LANGUE) %in% c(1,3)] <- 0
table(Clean$anglais)

table(Raw$LANGUE)
Clean$francais <- NA
Clean$francais[as.numeric(Raw$LANGUE) == 1] <- 1
Clean$francais[as.numeric(Raw$LANGUE) %in% c(2,3)] <- 0
table(Clean$francais)

table(Raw$LANGUE)
Clean$langautre <- NA
Clean$langautre[as.numeric(Raw$LANGUE) == 3] <- 1
Clean$langautre[as.numeric(Raw$LANGUE) %in% c(1,2)] <- 0
table(Clean$langautre)
## riding ------------------------------------------------------------------

#### Load data from article_riding_volatility to get riding
riding_volatility_df <- readRDS("_SharedFolder_article_pot-growth/data/lake/riding_volatility_data.rds") %>% 
  # filter for june only
  filter(source_id == "june")
table(riding_volatility_df$riding_id)

## Create riding_id column in Clean
Clean$riding_id <- riding_volatility_df$riding_id

## Load riding names (to join on riding_id)
riding_names_df <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/dimensions/prov_ridings/data.rds") %>% 
  select(riding_id, riding_name)

### Join riding_name on riding_id
Clean <- left_join(Clean, riding_names_df, by = "riding_id")

## RCI ---------------------------------------------------------------------


# Save Clean to a rds dataset ---------------------------------------------

saveRDS(Clean, "_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/separated_prov/omnibus_june.rds")
