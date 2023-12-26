# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------

# ridings regions
Ridings <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  select(riding_id, granular)

# poststrat
Strat <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/dimensions/census/fedcan2021/poststrat.rds") %>% 
  left_join(., Ridings, by = "riding_id",
            relationship = "many-to-many") %>% 
  mutate(gender = ifelse(gender == "men", 1, 0)) %>% 
  rename(male = gender)

parties <- c("PLC", "PCC", "NPD", "BQ", "PVC")

# Predict RCI by disaggregated profile ------------------------------------

for (i in parties){
  model <- readRDS(paste0("_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_", i, ".rds"))
  Predsi <- marginaleffects::predictions(model, newdata = Strat,
                                         vcov = "HC3", # adjust for heteroskedasticity
                                         conf_level = 0.95) %>% 
    select(-starts_with("rci")) %>% 
    mutate(party = i)
  if (i == parties[1]){
    Preds <- Predsi
  } else {
    Preds <- rbind(Preds, Predsi)
  }
  message(i)
}

saveRDS(Preds, "_SharedFolder_article_pot-growth/data/marts/rci_by_riding/fedcan2021/disaggregated.rds")


