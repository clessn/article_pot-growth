# Load packages ----------------------------------------------------------------
library(dplyr)

# Load raw data -----------------------------------------------------------
Raw <- sondr::read_any_csv("_SharedFolder_article_pot-growth/data/lake/quorum_mcq_pilote/quorum_mcq_pilote.csv") %>% 
  filter(ses_province == "Québec" |
         EN_ses_province == "Quebec")

# Create empty clean dataframe --------------------------------------------
Clean <- data.frame(id = 1:nrow(Raw), # id of the respondent
                    source_id = "quorum_mcq_pilote", # id of the survey
                    year = 2023, # year of the survey
                    level = "prov_qc") # fed_can or prov_qc

# Clean variables ---------------------------------------------------------

## gender ------------------------------------------------------------------
table(Raw$EN_ses_gender)
table(Raw$ses_gender)
Clean$male <- NA
Clean$male[Raw$EN_ses_gender == "Woman" |
                       Raw$ses_gender == "Femme"] <- 0
Clean$male[Raw$EN_ses_gender == "Man" |
                       Raw$ses_gender == "Homme"] <- 1
table(Clean$male)

Clean$female <- NA

## age ------------------------------------------------------------------
table(Raw$EN_ses_age)
table(Raw$ses_age)
Raw$age <- coalesce(as.numeric(Raw$EN_ses_age), as.numeric(Raw$ses_age))
table(Raw$age)

Clean$age <- case_when(
  Raw$age %in% c(18:34) ~ "34m", # if QAGE est entre 18 et 34, mettre 34m
  Raw$age %in% c(35:54) ~ "3554", # if QAGE est entre 35 et 54, mettre 3554
  Raw$age %in% c(55:99) ~ "55p" # if QAGE est entre 55 et 100, mettre 55p
)
table(Clean$age)

# factorize (donc mettre en catégories)
Clean$age <- factor(Clean$age, levels = c("34m", "3554", "55p")) # levels permet d'ordonner la variable catégorielle

## language ------------------------------------------------------------------
table(Raw$ses_lang)
Raw$ses_lang[Raw$ses_lang == ""] <- NA
table(Raw$EN_ses_lang)
Raw$EN_ses_lang[Raw$EN_ses_lang == ""] <- NA

Raw$langue <- coalesce(Raw$EN_ses_lang, Raw$ses_lang)
table(Raw$langue)

Clean$langue <- case_when(
  Raw$langue %in% c("Anglais", "English") ~ "english", # if LANGUE est 2, mettre english
  Raw$langue %in% c("Français", "French") ~ "french", # if LANGUE est 1, mettre french
  Raw$langue %in% c("Autre", "Other") ~ "other" # if LANGUE est 3, mettre other
)
table(Clean$langue)

# factorize (mais sans ordonner la variable cette fois)
Clean$langue <- factor(Clean$langue)


## education -------------------------------------------------------------
table(Raw$ses_education)
Raw$ses_education[Raw$ses_education == ""] <- NA
table(Raw$EN_ses_education)
Raw$EN_ses_education[Raw$EN_ses_education == ""] <- NA

Raw$educ <- coalesce(Raw$EN_ses_education, Raw$ses_education)
table(Raw$educ)

Clean$educ <- case_when(
  Raw$educ %in% c("Aucune scolarité", "École primaire", "École secondaire",
                  "High school") ~ "bhs",
  Raw$educ %in% c("Collège, Cégep ou Collège classique",
                  "Technical, community college, CEGEP, or college classique") ~ "college",
  Raw$educ %in% c("Baccalauréat", "Bachelor's degree", "Doctorat", "Doctorate",
                  "Maîtrise", "Master's degree") ~ "univ"
)

Clean$educ <- factor(Clean$educ, levels = c("bhs", "college", "univ"))

## riding ------------------------------------------------------------------

pc_fr <- Raw$ses_postal_code_1_TEXT
pc_fr[pc_fr == ""] <- NA
pc_fr
pc_en <- Raw$EN_ses_postal_code_1_TEXT
pc_en[pc_en == ""] <- NA
pc_en

postal_codes_data <- tolower(substr(coalesce(pc_fr, pc_en), 1, 3))

#### associate each postal code to its possible ridings
postal_code_ridings <- read.csv("_SharedFolder_article_pot-growth/data/lake/census/provqc2022/postal_codes_to_ridings.txt",
                                sep = ";", encoding = "latin1") %>% 
  mutate(postal_code_id = tolower(substr(CO_POSTL, 1, 3))) %>% 
  select(postal_code_id,
         #ville = NM_MUNCP,
         #mrc = NM_MRC,
         #region_adm = NM_REGN_ADMNS,
         riding_id = CO_CIRC)

possible_ridings <- postal_code_ridings %>% 
  group_by(postal_code_id, riding_id) %>% 
  summarise(n = n()) %>% 
  group_by(postal_code_id) %>% 
  summarise(n_possible_ridings = n())

census_by_riding <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/dimensions/census/provqc2022/census.rds") %>% 
  filter(!(category %in% c("women", "french", "1834"))) %>% 
  mutate(var = paste0(var, "_", category),
         prop = n / total_pop) %>% 
  tidyr::pivot_wider(., id_cols = "riding_id",
                     names_from = "var", values_from = "prop")

## join n_possible_ridings and census
model_data <- postal_code_ridings %>% 
  left_join(., possible_ridings, by = "postal_code_id") %>% 
  left_join(., census_by_riding, by = "riding_id") %>% 
  filter(n_possible_ridings != 1)

#postal codes with multiple possible ridings
postal_codes_multiple <- unique(model_data$postal_code_id)
# postal codes with one possible riding
postal_codes_single <- unique(possible_ridings$postal_code_id[possible_ridings$n_possible_ridings == 1])

### associate each single postal code id to its riding
single_ridings_associate <- postal_code_ridings %>%
  filter(postal_code_id %in% postal_codes_single) %>% 
  group_by(postal_code_id) %>% 
  reframe(riding_id = unique(riding_id))

respondents_with_single_possible_riding <- data.frame(
  id = which(postal_codes_data %in% postal_codes_single),
  postal_code_id = postal_codes_data[which(postal_codes_data %in% postal_codes_single)]
) %>% 
  left_join(., single_ridings_associate, by = "postal_code_id") %>% 
  select(-postal_code_id)

### train models
Model_ridings <- model_data %>%
  mutate(riding_id = factor(riding_id)) %>%
  ### Relevel data ###
  tidyr::nest(data = -"postal_code_id") %>%
  ### Get regression models ###
  mutate(models = purrr::map(data, ~ nnet::multinom(riding_id ~ gender_men + age_3554 +
                                          age_55p + 
                                          langue_english + 
                                          langue_other,
                                          data = .x))) %>%
  select(-data)

datapred <- Clean %>% 
  mutate(postal_code_id = postal_codes_data) %>%
  rename(gender_men = male) %>% 
  fastDummies::dummy_cols(.,
                          select_columns = c("age", "langue"),
                          ignore_na = TRUE,
                          remove_selected_columns = TRUE) %>% 
  tidyr::nest(data = -postal_code_id) %>%
  left_join(., Model_ridings, by = "postal_code_id") %>% 
  filter(models != "NULL")

for (i in 1:nrow(datapred)){
  datai <- datapred[i,] %>% 
    mutate(pred = purrr::map(models, predict, type = 'class', newdata = data)) %>%
    tidyr::unnest(c(data, pred)) %>% 
    select(id, riding_id = pred)
  if (i == 1){
    respondents_with_multiple_possible_ridings <- datai
  } else {
    respondents_with_multiple_possible_ridings <- rbind(respondents_with_multiple_possible_ridings, datai)
  }
  message(i)
}

riding_of_respondents <- rbind(respondents_with_single_possible_riding,
                               respondents_with_multiple_possible_ridings)

Clean <- left_join(Clean, riding_of_respondents, by = "id") %>% 
  mutate(riding_id = as.numeric(riding_id))

riding_names_df <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/dimensions/prov_ridings/data.rds") %>% 
  select(riding_id, riding_name)

Clean <- left_join(Clean, riding_names_df, by = "riding_id")

## RCI ---------------------------------------------------------------------

Raw$op_potentialG_CAQ <- coalesce(Raw$pot_growth_prov_1, Raw$EN_pot_growth_prov_1)
Raw$op_potentialG_PLQ <- coalesce(Raw$pot_growth_prov_2, Raw$EN_pot_growth_prov_2)
Raw$op_potentialG_QS <- coalesce(Raw$pot_growth_prov_3, Raw$EN_pot_growth_prov_3)
Raw$op_potentialG_PQ <- coalesce(Raw$pot_growth_prov_4, Raw$EN_pot_growth_prov_4)
Raw$op_potentialG_PCQ <- coalesce(Raw$pot_growth_prov_5, Raw$EN_pot_growth_prov_5)

rcis <- Raw %>%
  select(
    starts_with("op_potential")
  ) %>%
  mutate(id = 1:nrow(.),
         op_potentialG_CAQ = op_potentialG_CAQ / 10,
         op_potentialG_PLQ = op_potentialG_PLQ / 10,
         op_potentialG_PQ = op_potentialG_PQ / 10,
         op_potentialG_QS = op_potentialG_QS / 10,
         op_potentialG_PCQ = op_potentialG_PCQ / 10) %>%
  tidyr::pivot_longer(
    .,
    cols = starts_with("op_potentialG"),
    names_to = "party",
    values_to = "potgrowth",
    names_prefix = "op_potentialG_"
  ) %>%
  group_by(id) %>%
  mutate(
    ### if respondent answered some parties but not others, it means that in
    ### Qualtrics the respondent didnt move the cursor from 5 
    nnas = clessnverse::count_na(potgrowth),
    potgrowth = ifelse(nnas == 1 & is.na(potgrowth), 0.5, potgrowth)
  ) %>% 
  select(-nnas) %>% 
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
  tidyr::pivot_wider(., id_cols = "id",
              values_from = "rci",
              names_from = "party",
              names_prefix = "rci_") %>% 
  ungroup() %>% 
  select(-id)

Clean <- cbind(Clean, rcis)

# Save Clean to a rds dataset ---------------------------------------------

saveRDS(Clean, "_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/prov_2023/quorum_mcq_pilote.rds")
saveRDS(Clean, "_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/separated_prov/quorum_mcq_pilote.rds")
