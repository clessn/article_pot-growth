# Packages -------------------
library(dplyr)

# Data  -------------------
Data <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/step1_people_predict/appdatagotchi22.rds")

# Wrangling -------------------------------------------------------------------------

Long <- Data %>% 
  tidyr::pivot_longer(., cols = starts_with("people_pred"),
                      names_to = "party",
                      values_to = "pred",
                      names_prefix = "people_pred_") %>% 
  filter(pred == 1) %>% 
  ### arbitrary weights based on education. Does not change much in the agregate.
  mutate(weight = case_when(
    pol_sophis == 1 ~ 0.7,
    pol_sophis == 0.5 ~ 0.5,
    pol_sophis == 0 ~ 0.3,
  ))

ByRiding <- Long %>% 
  group_by(riding_id, riding_name, party) %>% 
  summarise(weighted_sum_preds = sum(weight)) %>% 
  group_by(riding_id, riding_name) %>% 
  mutate(total_riding = sum(weighted_sum_preds),
         prop_forecasted = weighted_sum_preds / total_riding) %>% 
  select(-weighted_sum_preds, -total_riding)

saveRDS(ByRiding, "_SharedFolder_article_pot-growth/data/marts/predictions_by_riding/provqc2022.rds")

