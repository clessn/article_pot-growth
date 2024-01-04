# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------

# ridings regions
Ridings <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  select(riding_id, granular)

# poststrat
Strat <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/dimensions/census/provqc2022/poststrat.rds") %>% 
  left_join(., Ridings, by = "riding_id") %>% 
  mutate(gender = ifelse(gender == "men", 1, 0),
         granular = factor(granular)) %>% 
  rename(male = gender)

parties <- c("CAQ", "PLQ", "QS", "PQ", "PCQ")

# Predict models ----------------------------------------------------------

#https://stats.stackexchange.com/questions/56895/do-the-predictions-of-a-random-forest-model-have-a-prediction-interval

## Vote solidity -----------------------------------------------------------

for (i in parties){
  model <- readRDS(paste0("code/analysis/step3_aggregate_rci/generate_models/models/solidity_", i, ".rds"))
  pred.rf <- predict(model, newdata = Strat,
                     predict.all = TRUE)
  pred.rf.int <- apply(pred.rf$individual, 1, function(x) {
    c(quantile(x, c(0.25, 0.75))) ## 50% interval, we want the 50% trees in the middle.
  })
  Predsi <- as.data.frame(t(pred.rf.int)) %>% 
    rename(conf.low = "25%", conf.high = "75%") %>%
    cbind(Strat, .) %>% 
    ungroup() %>% 
    mutate(estimate = unlist(pred.rf$aggregate),
           party = i,
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
  model <- readRDS(paste0("code/analysis/step3_aggregate_rci/generate_models/models/potgrowth_", i, ".rds"))
  pred.rf <- predict(model, newdata = Strat,
                     predict.all = TRUE)
  pred.rf.int <- apply(pred.rf$individual, 1, function(x) {
    c(quantile(x, c(0.25, 0.75))) ## 50% interval, we want the 50% trees in the middle.
  })
  Predsi <- as.data.frame(t(pred.rf.int)) %>% 
    rename(conf.low = "25%", conf.high = "75%") %>%
    cbind(Strat, .) %>% 
    ungroup() %>% 
    mutate(estimate = unlist(pred.rf$aggregate),
           party = i,
           model = "potgrowth")
  if (i == parties[1]){
    PredsPotGrowth <- Predsi
  } else {
    PredsPotGrowth <- rbind(PredsPotGrowth, Predsi)
  }
  message(i)
}

## Vote int ----------------------------------------------------------------

model <- readRDS("_SharedFolder_article_pot-growth/data/marts/models/provqc2022/voteint/model.rds")

PredsVoteInt <- cbind(Strat, predict(model, newdata = Strat, type = "probs")) %>% 
  tidyr::pivot_longer(., cols = all_of(parties),
                      names_to = "party",
                      values_to = "predicted_vote_share")

saveRDS(PredsVoteInt, "_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2022/disaggregated/voteint.rds")

# Data wrangling ----------------------------------------------------------

Preds <- rbind(PredsVoteSol, PredsPotGrowth)

saveRDS(Preds, "code/analysis/step3_aggregate_rci/generate_models/models/disaggregated_potgrowth_votesol.rds")
