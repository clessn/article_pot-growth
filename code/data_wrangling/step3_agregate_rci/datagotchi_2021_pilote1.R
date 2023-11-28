# Load packages ----------------------------------------------------------------
library(tidyverse)
library (sondr)

# Load raw data -----------------------------------------------------------
Raw <- readRDS("_SharedFolder_article_pot-growth/data/lake/datagotchi_2021_pilote1/CleanData-Lifestyle.rds")

# Create empty clean dataframe --------------------------------------------
Clean <- data.frame(id = 1:nrow(Raw), # id of the respondent
                    source_id = "datagotchi_2021_pilote1", # id of the survey
                    year = 2021, # year of the survey
                    level = "fed_can") # fed_can or prov_qc

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
table(Raw$age34m)
Clean$age34m <- NA
Clean$age34m[as.numeric(Raw$age34m) == 1] <- 1
Clean$age34m[as.numeric(Raw$age34m) == 0] <- 0
table(Clean$age34m)

table(Raw$age3554)
Clean$age3554 <- NA
Clean$age3554[as.numeric(Raw$age3554) == 1] <- 1
Clean$age3554[as.numeric(Raw$age3554) == 0] <- 0
table(Clean$age3554)

table(Raw$age55p)
Clean$age55p <- NA
Clean$age55p[as.numeric(Raw$age55p) == 1] <- 1
Clean$age55p[as.numeric(Raw$age55p) == 0] <- 0
table(Clean$age55p)

## language ------------------------------------------------------------------
table(Raw$langEn)
Clean$anglais <- NA
Clean$anglais[as.numeric(Raw$langEn) == 1] <- 1
Clean$anglais[as.numeric(Raw$langEn) == 0] <- 0
table(Clean$anglais)

table(Raw$langFr)
Clean$francais <- NA
Clean$francais[as.numeric(Raw$langFr) == 1] <- 1
Clean$francais[as.numeric(Raw$langFr) == 0] <- 0
table(Clean$francais)

table(Raw$ses_languageOther)
Clean$langautre <- NA
Clean$langautre[as.numeric(Raw$ses_languageOther) == 1] <- 1
Clean$langautre[as.numeric(Raw$ses_languageOther) == 0] <- 0
table(Clean$langautre)

## riding ------------------------------------------------------------------

## RCI ---------------------------------------------------------------------

rcis <- Raw %>% 
  select(op_potentialG_Lib, op_potentialG_Cons,
         op_potentialG_Ndp, op_potentialG_BQ, op_potentialG_PV) %>% 
  mutate(id = 1:nrow(.)) %>% 
  pivot_longer(., cols = starts_with("op_potentialG"),
               names_to = "party",
               values_to = "potgrowth",
               names_prefix = "op_potentialG_") %>% 
  group_by(id) %>% 
  mutate(max_potgrowth = max(potgrowth)) %>% 
  ungroup() %>% 
  mutate(rci)
  

# Save Clean to a rds dataset ---------------------------------------------

saveRDS(Clean, "_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/separated/")
