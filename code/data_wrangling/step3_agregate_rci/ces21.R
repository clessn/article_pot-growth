# Load packages ----------------------------------------------------------------
library(dplyr)

# Load raw data -----------------------------------------------------------
Raw <- sondr::read_any_csv("_SharedFolder_article_pot-growth/data/lake/ces2021/ces2021.csv")

# Create empty clean dataframe --------------------------------------------
Clean <- data.frame(id = 1:nrow(Raw), # id of the respondent
                    source_id = "ces21", # id of the survey
                    year = 2022, # year of the survey
                    level = "fed_can") # fed_can or prov_qc

# Clean variables ---------------------------------------------------------

## gender ------------------------------------------------------------------

Clean$male <- NA
Clean$male[Raw$cps21_genderid == "A man"] <- 1
Clean$male[Raw$cps21_genderid != "A man"] <- 0
table(Clean$male)

Clean$female <- NA
Clean$female[Raw$cps21_genderid == "A woman"] <- 1
Clean$female[Raw$cps21_genderid != "A woman"] <- 0
table(Clean$female)

## age ------------------------------------------------------------------
table(Raw$cps21_age)
class(Raw$cps21_age)

Clean$age <- case_when(
  as.numeric(Raw$cps21_age) %in% 18:34 ~ "34m", # if QAGE est entre 18 et 34, mettre 34m
  as.numeric(Raw$cps21_age) %in% 35:54 ~ "3554", # if QAGE est entre 35 et 54, mettre 3554
  as.numeric(Raw$cps21_age) %in% 55:100 ~ "55p" # if QAGE est entre 55 et 100, mettre 55p
)
table(Clean$age)

# factorize (donc mettre en catégories)
Clean$age <- factor(Clean$age, levels = c("34m", "3554", "55p")) # levels permet d'ordonner la variable catégorielle

## language ------------------------------------------------------------------

table(Raw$cps21_language_1, Raw$cps21_language_2)
table(Raw$UserLanguage)

Clean$langue <- case_when(
  Raw$UserLanguage == "FR-CA" ~ "french", 
  Raw$UserLanguage == "EN" &
    Raw$cps21_language_1 == 1 ~ "english",
  Raw$cps21_language_1 == "-99" & 
    Raw$cps21_language_2 == "-99" ~ "other",
  Raw$cps21_language_1 == "-99" &
    Raw$cps21_language_2 == "1" &
    Raw$UserLanguage == "EN" ~ "french"
)

table(Clean$langue, useNA = "always")

# factorize (mais sans ordonner la variable cette fois)
Clean$langue <- factor(Clean$langue)

table(Clean$langue)

## riding ------------------------------------------------------------------

table(Raw$feduid)
table(Raw$fedname)

Clean$riding_id <- Raw$feduid
Clean$riding_name <- Raw$fedname

Encoding(Clean$riding_name) <- "latin1"


## RCI ---------------------------------------------------------------------

rcis <- Raw %>%
  mutate(id = 1:nrow(.)) %>% 
  select(id, starts_with("cps21_party_rating"),
         -starts_with("cps21_party_rating_DO")) %>% 
  pivot_longer(., cols = starts_with("cps21_party_rating"),
               values_to = "potgrowth",
               names_to = "party",
               names_prefix = "cps21_party_rating_") %>% 
  ## if no answer (NA or -99) => remove this party for the respondent
  filter(potgrowth != "-99") %>% 
  mutate(potgrowth = as.numeric(potgrowth),
         potgrowth = round(potgrowth/10),
         party = case_when(
           party == "23" ~ "PLC",
           party == "24" ~ "PCC",
           party == "25" ~ "NPD",
           party == "26" ~ "BQ",
           party == "27" ~ "PVC",
           party == "29" ~ "PPC",
         )) %>% 
  group_by(id) %>%
  mutate(
    max_potgrowth = max(potgrowth, na.rm = TRUE),
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
    ),
    rci = rci / 10
  ) %>% 
  select(id, party, rci) %>% 
  pivot_wider(., id_cols = "id",
              values_from = "rci",
              names_from = "party",
              names_prefix = "rci_") %>% 
  ungroup()

Clean <- left_join(Clean, rcis, by = "id")

# Save Clean to a rds dataset ---------------------------------------------

saveRDS(Clean, "_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/separated_fed/ces21.rds")
