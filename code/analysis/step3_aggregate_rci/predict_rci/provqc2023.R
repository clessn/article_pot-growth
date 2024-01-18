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

# Predict models ----------------------------------------------------------

## Vote solidity -----------------------------------------------------------

for (i in parties){
  model <- readRDS(paste0("_SharedFolder_article_pot-growth/data/marts/models/provqc2023/vote_solidity/model_", i, ".rds"))
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
  model <- readRDS(paste0("_SharedFolder_article_pot-growth/data/marts/models/provqc2023/potgrowth/model_", i, ".rds"))
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

model <- readRDS("_SharedFolder_article_pot-growth/data/marts/models/provqc2023/voteint/model.rds")

PredsVoteInt <- cbind(Strat, predict(model, newdata = Strat, type = "probs")) %>% 
  tidyr::pivot_longer(., cols = all_of(parties),
                      names_to = "party",
                      values_to = "predicted_vote_share") %>% 
  select(-n, -prct, -granular)

saveRDS(PredsVoteInt, "_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2023/disaggregated/voteint.rds")

# Data wrangling ----------------------------------------------------------

Preds <- rbind(PredsVoteSol, PredsPotGrowth)

saveRDS(Preds, "_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2023/disaggregated/potgrowth_votesolidity.rds")
