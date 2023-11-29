# Load packages ----------------------------------------------------------------
library(dplyr)

# Load raw data -----------------------------------------------------------

## in this survey, some respondents were removed in the cleaning. We need to remove them from Raw.
clean_survey <- sondr::read_any_csv("_SharedFolder_article_pot-growth/data/lake/datagotchi_2022_pilote1/Pilote1_clean.csv")
clean_survey_ix <- clean_survey$id ## vector containing the rows to keep

Raw <- haven::read_sav("_SharedFolder_article_pot-growth/data/lake/datagotchi_2022_pilote1/ULA12-BASE-1500.sav") %>% 
  ## only keep rows from clean_survey_ix
  slice(clean_survey_ix)

# Create empty clean dataframe --------------------------------------------
Clean <- data.frame(id = 1:nrow(Raw), # id of the respondent
                    source_id = "datagotchi_2022_pilote1", # id of the survey
                    year = 2022, # year of the survey
                    level = "prov_qc") # fed_can or prov_qc

# Clean variables ---------------------------------------------------------

## gender ------------------------------------------------------------------
table(Raw$SEXE)
Clean$male <- NA
Clean$male[as.numeric(Raw$SEXE) %in% c(1, 3)] <- 1
Clean$male[!(as.numeric(Raw$SEXE) %in% c(1, 3))] <- 0
table(Clean$male)

table(Raw$SEXE)
Clean$female <- NA
Clean$female[as.numeric(Raw$SEXE) %in% c(2, 4)] <- 1
Clean$female[!(as.numeric(Raw$SEXE) %in% c(2, 4))] <- 0
table(Clean$female)

## age ------------------------------------------------------------------
table(Raw$QAGE)
class(Raw$QAGE)

# clean variable
Clean$age <- case_when(
  Raw$QAGE %in% 18:34 ~ "34m", # if QAGE est entre 18 et 34, mettre 34m
  Raw$QAGE %in% 35:54 ~ "3554", # if QAGE est entre 35 et 54, mettre 3554
  Raw$QAGE %in% 55:100 ~ "55p" # if QAGE est entre 55 et 100, mettre 55p
)
table(Clean$age)

# factorize (donc mettre en catégories)
Clean$age <- factor(Clean$age, levels = c("34m", "3554", "55p")) # levels permet d'ordonner la variable catégorielle

## language ------------------------------------------------------------------

table(Raw$LANGU)

Clean$langue <- case_when(
  Raw$LANGU == 1 ~ "english", # if LANGU est 1, mettre english
  Raw$LANGU == 2 ~ "french", # if QAGE est 2, mettre french
  Raw$LANGU == 3 ~ "other" # if QAGE est 3, mettre other
)

table(Clean$langue)

# factorize (mais sans ordonner la variable cette fois)
Clean$langue <- factor(Clean$langue)

## riding ------------------------------------------------------------------

#### Load data from article_riding_volatility to get riding
riding_volatility_df <- readRDS("_SharedFolder_article_pot-growth/data/lake/riding_volatility_data.rds") %>% 
  # filter for datagotchi_2022_pilote1 only
  filter(source_id == "pilote1")
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

saveRDS(Clean, "_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/separated_prov/datagotchi_2022_pilote1.rds")
