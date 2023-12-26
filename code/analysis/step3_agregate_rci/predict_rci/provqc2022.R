# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------

# ridings regions
Ridings <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  select(riding_id, granular)

# poststrat
Strat <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/dimensions/census/provqc2022/poststrat.rds") %>% 
  left_join(., Ridings, by = "riding_id") %>% 
  mutate(gender = ifelse(gender == "men", 1, 0)) %>% 
  rename(male = gender)

parties <- c("CAQ", "PLQ", "QS", "PQ", "PCQ")

# Predict RCI by disaggregated profile ------------------------------------

for (i in parties){
  model <- readRDS(paste0("_SharedFolder_article_pot-growth/data/marts/models/provqc2022/model_", i, ".rds"))
  Predsi <- marginaleffects::predictions(model, newdata = Strat,
                                         conf_level = 0.95) %>% 
    select(-starts_with("rci")) %>% 
    mutate(party = i)
  if (i == parties[1]){
    Preds <- Predsi
  } else {
    Preds <- rbind(Preds, Predsi)
  }
}

saveRDS(Preds, "_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2022/disaggregated.rds")


