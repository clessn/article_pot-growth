# Load packages ----------------------------------------------------------------
library(tidyverse)

# Load raw data -----------------------------------------------------------
Raw <- readRDS("_SharedFolder_article_pot-growth/data/lake/datagotchi_2021_pilote1/CleanData-Lifestyle.rds")

# Create empty clean dataframe --------------------------------------------
Clean <- data.frame(id = 1:nrow(Raw), # id of the respondent
                    source_id = "datagotchi_2021_pilote1", # id of the survey
                    year = 2021, # year of the survey
                    level = "fed_can") # fed_can or prov_qc

# Clean variables ---------------------------------------------------------

## gender ------------------------------------------------------------------

## age ------------------------------------------------------------------

## language ------------------------------------------------------------------

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
