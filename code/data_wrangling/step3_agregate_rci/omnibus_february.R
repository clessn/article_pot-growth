# Load packages ----------------------------------------------------------------
library(tidyverse)
library(sondr)

# Load raw data -----------------------------------------------------------
Raw <- haven::read_sav("_SharedFolder_article_pot-growth/data/lake/omnibus/february/february.Sav")

# Create empty clean dataframe --------------------------------------------
Clean <- data.frame(id = 1:nrow(Raw), # id of the respondent
                    source_id = "omnibus_february", # id of the survey
                    year = 2022, # year of the survey
                    level = "prov_qc") # fed_can or prov_qc

# Clean variables ---------------------------------------------------------

## gender ---------------------------------------------------------------
table(Raw$SEXE)
Clean$male <- NA
Clean$male[as.numeric(Raw$SEXE) %in% c(1)] <- 1
Clean$male[!(as.numeric(Raw$SEXE) %in% c(1))] <- 0
table(Clean$male)

table(Raw$SEXE)
Clean$female <- NA
Clean$female[as.numeric(Raw$SEXE) %in% c(2)] <- 1
Clean$female[!(as.numeric(Raw$SEXE) %in% c(2))] <- 0
table(Clean$female)

## age ------------------------------------------------------------------
attributes(Raw$AGE)
table(Raw$AGE)
class(Raw$AGE)

# clean variable
Clean$age <- case_when(
  Raw$AGE %in% c("2","3") ~ "34m", # if QAGE est entre 18 et 34, mettre 34m
  Raw$AGE %in% c("4","5") ~ "3554", # if QAGE est entre 35 et 54, mettre 3554
  Raw$AGE %in% c("6","7","8") ~ "55p" # if QAGE est entre 55 et 100, mettre 55p
)
table(Clean$age)

# factorize (donc mettre en catégories)
Clean$age <- factor(Clean$age, levels = c("34m", "3554", "55p")) # levels permet d'ordonner la variable catégorielle

## language ------------------------------------------------------------------
table(Raw$LANGUE)

Clean$langue <- case_when(
  Raw$LANGUE == 2 ~ "english", # if LANGUE est 2, mettre english
  Raw$LANGUE == 1 ~ "french", # if LANGUE est 1, mettre french
  Raw$LANGUE == 3 ~ "other" # if LANGUE est 3, mettre other
)

table(Clean$langue)

# factorize (mais sans ordonner la variable cette fois)
Clean$langue <- factor(Clean$langue)

## educ ------------------------------------------------------------------
table(Raw$S2)
attributes(Raw$S2)

Clean$educ <- case_when(
  Raw$S2 == 1 ~ "bhs",
  Raw$S2 == 2 ~ "college",
  Raw$S2 == 3 ~ "univ"
)

Clean$educ <- factor(Clean$educ, levels = c("bhs", "college", "univ"))

## riding ------------------------------------------------------------------

#### Load data from article_riding_volatility to get riding
riding_volatility_df <- readRDS("_SharedFolder_article_pot-growth/data/lake/riding_volatility_data.rds") %>% 
  # filter for february only
  filter(source_id == "february")
table(riding_volatility_df$riding_id)

## Create riding_id column in Clean
Clean$riding_id <- riding_volatility_df$riding_id

## Load riding names (to join on riding_id)
riding_names_df <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/dimensions/prov_ridings/data.rds") %>% 
  select(riding_id, riding_name)

### Join riding_name on riding_id
Clean <- left_join(Clean, riding_names_df, by = "riding_id")


## RCI ---------------------------------------------------------------------

rcis <- Raw %>%
  select(
    C3_A1,
    C3_A2,
    C3_A3,
    C3_A4,
    C3_A5
  ) %>%
  rename(
    op_potentialG_CAQ = C3_A1,
    op_potentialG_PLQ = C3_A2,
    op_potentialG_PQ =  C3_A3,
    op_potentialG_QS =  C3_A4,
    op_potentialG_PCQ = C3_A5
  ) %>% 
  mutate(id = 1:nrow(.),
         op_potentialG_CAQ = op_potentialG_CAQ / 10,
         op_potentialG_PLQ = op_potentialG_PLQ / 10,
         op_potentialG_PQ = op_potentialG_PQ / 10,
         op_potentialG_QS = op_potentialG_QS / 10,
         op_potentialG_PCQ = op_potentialG_PCQ / 10) %>%
  pivot_longer(
    .,
    cols = starts_with("op_potentialG"),
    names_to = "party",
    values_to = "potgrowth",
    names_prefix = "op_potentialG_"
  ) %>%
  group_by(id) %>%
  mutate(
    max_potgrowth = max(potgrowth),
    leader = ifelse(potgrowth == max_potgrowth, 1, 0),
    trailer = ifelse(potgrowth != max_potgrowth, 1, 0),
    n_leaders = sum(leader),
    potgrowth_trailers = ifelse(trailer == 1, potgrowth, NA),
    second_potgrowth = case_when(
      n_leaders == 1 ~ max(potgrowth_trailers, na.rm = TRUE),
      n_leaders >= 2 ~ max_potgrowth
    ),
    rci = case_when(
      leader == 1 ~ potgrowth - second_potgrowth,
      trailer == 1 ~ potgrowth - max_potgrowth
    )
  ) %>% 
  select(id, party, rci) %>% 
  pivot_wider(., id_cols = "id",
              values_from = "rci",
              names_from = "party",
              names_prefix = "rci_") %>% 
  ungroup() %>% 
  select(-id)

Clean <- cbind(Clean, rcis)

# Save Clean to a rds dataset ---------------------------------------------

saveRDS(Clean, "_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/separated_prov/omnibus_february.rds")
