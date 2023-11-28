# Load packages ----------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(sondr)

# Load raw data -----------------------------------------------------------

## in this survey, some respondents were removed in the cleaning. We need to remove them from Raw.
clean_survey <- sondr::read_any_csv("_SharedFolder_article_pot-growth/data/lake/datagotchi_2022_pilote1/Pilote1_clean.csv")
clean_survey_ix <- clean_survey$id ## vector containing the rows to keep

Raw <- sondr::read_any_csv("_SharedFolder_article_pot-growth/data/lake/datagotchi_2022_pilote2/datagotchi_pilot2_2022.csv")

# Create empty clean dataframe --------------------------------------------
Clean <- data.frame(id = 1:nrow(Raw), # id of the respondent
                    source_id = "datagotchi_2022_pilote2", # id of the survey
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
annee_sondage <- 2022
Clean$age <- annee_sondage - as.numeric(Raw$age)
Clean$age34m <- ifelse(Clean$age <= 34, 1, 0)
table(Clean$age34m)

Clean$age3554 <- ifelse(Clean$age >= 35 & Clean$age <= 54, 1, 0)
table(Clean$age3554)

Clean$age55p <- ifelse(Clean$age >= 55, 1, 0)
table(Clean$age55p)

## language ------------------------------------------------------------------
table(Raw$language)
Clean$anglais <- NA
Clean$anglais[as.numeric(Raw$language) == 1] <- 1
Clean$anglais[as.numeric(Raw$language) == 2] <- 0
Clean$anglais[as.numeric(Raw$language) == 3] <- 0
table(Clean$anglais)

Clean$francais <- NA
Clean$francais[as.numeric(Raw$language) == 2] <- 1
Clean$francais[as.numeric(Raw$language) == 1] <- 0
Clean$francais[as.numeric(Raw$language) == 3] <- 0
table(Clean$francais)

table(Raw$language)
Clean$langautre <- NA
Clean$langautre[as.numeric(Raw$language) == 3] <- 1
Clean$langautre[as.numeric(Raw$language) == 1] <- 0
Clean$langautre[as.numeric(Raw$language) == 2] <- 0
table(Clean$langautre)


## riding ------------------------------------------------------------------

#### Load data from article_riding_volatility to get riding
riding_volatility_df <- readRDS("_SharedFolder_article_pot-growth/data/lake/riding_volatility_data.rds") %>% 
  # filter for datagotchi_2022_pilote1 only
  filter(source_id == "pilote2")
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

saveRDS(Clean, "_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/separated_prov/datagotchi_2022_pilote2.rds")

