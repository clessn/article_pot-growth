# Load packages ----------------------------------------------------------------


# Load raw data -----------------------------------------------------------
Raw <- haven::read_sav("_SharedFolder_article_pot-growth/data/lake/omnibus/january/january.Sav")

# Create empty clean dataframe --------------------------------------------
Clean <- data.frame(id = 1:nrow(Raw), # id of the respondent
                    source_id = "omnibus_january", # id of the survey
                    year = 2022, # year of the survey
                    level = "prov_qc") # fed_can or prov_qc

# Clean variables ---------------------------------------------------------

## riding ------------------------------------------------------------------

## indicator of political sophistication ------------------------------------------------------------------

## riding prediction ---------------------------------------------------------------------


# Save Clean to a rds dataset ---------------------------------------------

saveRDS(Clean, "_SharedFolder_article_pot-growth/data/warehouse/step1_people_predict/")
