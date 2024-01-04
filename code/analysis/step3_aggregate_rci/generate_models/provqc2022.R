# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/rcis_prov.rds")
Ridings <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  select(riding_id, large, granular)

# Join riding region on Data
Data <- left_join(Data, Ridings, by = "riding_id") %>% 
  mutate(riding_id = factor(riding_id),
         large = factor(large),
         granular = factor(granular),
         age = factor(age),
         langue = factor(langue))

## create vector of independent variables
independent_vars <- c("male", "age", "langue", "granular")

## vote solidity
Solidity <- Data %>% 
  tidyr::pivot_longer(., cols = starts_with("rci"),
                      names_to = "party", names_prefix = "rci_",
                      values_to = "rci") %>% 
  group_by(id, source_id) %>% 
  filter(rci == max(rci)) %>% 
  tidyr::drop_na(all_of(independent_vars))

## potential for growth
Potgrowth <- Data %>% 
  tidyr::pivot_longer(., cols = starts_with("rci"),
                      names_to = "party", names_prefix = "rci_",
                      values_to = "rci") %>% 
  group_by(id, source_id) %>% 
  filter(rci != max(rci) | rci == 0) %>% 
  tidyr::drop_na(all_of(independent_vars))

## vote intent
Voteint <- Solidity %>%
  # remove undecided voters
  filter(rci != 0)

# Train models ------------------------------------------------------------------

## function to train model of party
custom_random_forest <- function(party, data){
  model <- randomForest::tuneRF(
    x = data[data$party == party, independent_vars],
    y = data[data$party == party,]$rci,
    doBest = TRUE,
    stepFactor = 1.2,
    improve = 0.01,
    trace = T,
    ntreeTry = 501
  )
  return(model)
}

## Vote solidity -----------------------------------------------------------

model_CAQ_solidity <- custom_random_forest(party = "CAQ",
                                           data = Solidity)
saveRDS(model_CAQ_solidity, "code/analysis/step3_aggregate_rci/generate_models/models/solidity_CAQ.rds")

# Pour le PLQ
model_PLQ_solidity <- custom_random_forest(party = "PLQ", data = Solidity)
saveRDS(model_PLQ_solidity, "code/analysis/step3_aggregate_rci/generate_models/models/solidity_PLQ.rds")

# Pour le QS
model_QS_solidity <- custom_random_forest(party = "QS", data = Solidity)
saveRDS(model_QS_solidity, "code/analysis/step3_aggregate_rci/generate_models/models/solidity_QS.rds")

# Pour le PQ
model_PQ_solidity <- custom_random_forest(party = "PQ", data = Solidity)
saveRDS(model_PQ_solidity, "code/analysis/step3_aggregate_rci/generate_models/models/solidity_PQ.rds")

# Pour le PCQ
model_PCQ_solidity <- custom_random_forest(party = "PCQ", data = Solidity)
saveRDS(model_PCQ_solidity, "code/analysis/step3_aggregate_rci/generate_models/models/solidity_PCQ.rds")

## Potential for growth --------------------------------------------------

# Pour le CAQ
model_CAQ_potgrowth <- custom_random_forest(party = "CAQ", data = Potgrowth)
saveRDS(model_CAQ_potgrowth, "code/analysis/step3_aggregate_rci/generate_models/models/potgrowth_CAQ.rds")

# Pour le PLQ
model_PLQ_potgrowth <- custom_random_forest(party = "PLQ", data = Potgrowth)
saveRDS(model_PLQ_potgrowth, "code/analysis/step3_aggregate_rci/generate_models/models/potgrowth_PLQ.rds")

# Pour le QS
model_QS_potgrowth <- custom_random_forest(party = "QS", data = Potgrowth)
saveRDS(model_QS_potgrowth, "code/analysis/step3_aggregate_rci/generate_models/models/potgrowth_QS.rds")

# Pour le PQ
model_PQ_potgrowth <- custom_random_forest(party = "PQ", data = Potgrowth)
saveRDS(model_PQ_potgrowth, "code/analysis/step3_aggregate_rci/generate_models/models/potgrowth_PQ.rds")

# Pour le PCQ
model_PCQ_potgrowth <- custom_random_forest(party = "PCQ", data = Potgrowth)
saveRDS(model_PCQ_potgrowth, "code/analysis/step3_aggregate_rci/generate_models/models/potgrowth_PCQ.rds")

## Vote int --------------------------------------------------------------

model <- nnet::multinom(party ~ age * langue * granular + male, Voteint)
saveRDS(model, "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/voteint/model.rds")
