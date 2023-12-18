# Packages -------------------
library(dplyr)

# Data  -------------------
Data <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/step1_people_predict/ces21.rds") %>% 
  ### put 0 weights at 0.05
  mutate(weight = ifelse(pol_sophis == 0, 0.05, pol_sophis))

# Wrangling -------------------------------------------------------------------------

Long <- Data %>% 
  tidyr::pivot_longer(., cols = starts_with("people_pred"),
                      names_to = "party",
                      values_to = "pred",
                      names_prefix = "people_pred_") %>%
  tidyr::drop_na(pred)

WeightsByRiding <- Data %>%
  ### generate sum of weight by riding
  group_by(riding_id) %>% 
  summarise(total_weight = sum(weight, na.rm = TRUE))

ByRiding <- Long %>% 
  ## weighted pred
  mutate(weighted_pred = weight * pred) %>% 
  group_by(riding_id, riding_name, party) %>% 
  summarise(raw_pred = sum(weighted_pred, na.rm = TRUE)) %>% 
  left_join(., WeightsByRiding, by = "riding_id") %>% 
  mutate(people_pred = raw_pred / total_weight) %>% 
  ## agregate so that all predictions add up to 1 in each riding
  group_by(riding_id) %>% 
  mutate(people_pred = people_pred / sum(people_pred)) %>% 
  ungroup() %>% 
  select(-raw_pred, -total_weight)

saveRDS(ByRiding, "_SharedFolder_article_pot-growth/data/marts/predictions_by_riding/fedcan2021.rds")
