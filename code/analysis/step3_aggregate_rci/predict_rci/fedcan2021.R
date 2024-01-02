# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------

# ridings regions
Ridings <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  select(riding_id, granular, large, density) %>% 
  mutate(large_density = paste0(large, "_", density))

# poststrat
Strat <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/dimensions/census/fedcan2021/poststrat.rds") %>% 
  left_join(., Ridings, by = "riding_id",
            relationship = "many-to-many") %>% 
  mutate(gender = ifelse(gender == "men", 1, 0)) %>% 
  rename(male = gender)

parties <- c("PLC", "PCC", "NPD", "BQ", "PVC")

# Predict models ----------------------------------------------------------

## Vote solidity -----------------------------------------------------------

for (i in parties){
  model <- readRDS(paste0("_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/vote_solidity/model_", i, ".rds"))
  Predsi <- marginaleffects::predictions(model, newdata = Strat,
                                         conf_level = 0.95) %>% 
    select(-starts_with("rci")) %>% 
    mutate(party = i,
           model = "vote_solidity")
  if (i == parties[1]){
    PredsVoteSol <- Predsi
  } else {
    PredsVoteSol <- rbind(PredsVoteSol, Predsi)
  }
  message(i)
}

## Potential for growth ----------------------------------------------------

for (i in parties){
  model <- readRDS(paste0("_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/potgrowth/model_", i, ".rds"))
  Predsi <- marginaleffects::predictions(model, newdata = Strat,
                                         conf_level = 0.95) %>% 
    select(-starts_with("rci")) %>% 
    mutate(party = i,
           model = "potgrowth")
  if (i == parties[1]){
    PredsPotGrowth <- Predsi
  } else {
    PredsPotGrowth <- rbind(PredsPotGrowth, Predsi)
  }
  message(i)
}

## Vote int ----------------------------------------------------------------

model <- readRDS("_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/voteint/model.rds")

PredsVoteInt <- cbind(Strat, predict(model, newdata = Strat, type = "probs")) %>% 
  tidyr::pivot_longer(., cols = all_of(parties),
                      names_to = "party",
                      values_to = "predicted_vote_share") %>% 
  select(-n, -prct, -granular)

saveRDS(PredsVoteInt, "_SharedFolder_article_pot-growth/data/marts/rci_by_riding/fedcan2021/disaggregated/voteint.rds")

# Data wrangling ----------------------------------------------------------

Preds <- rbind(PredsVoteSol, PredsPotGrowth)

saveRDS(Preds, "_SharedFolder_article_pot-growth/data/marts/rci_by_riding/fedcan2021/disaggregated/potgrowth_votesolidity.rds")
