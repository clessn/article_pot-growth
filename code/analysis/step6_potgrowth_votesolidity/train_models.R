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


## vote solidity
Solidity <- Data %>% 
  tidyr::pivot_longer(., cols = starts_with("rci"),
                      names_to = "party", names_prefix = "rci_",
                      values_to = "rci") %>% 
  group_by(id, source_id) %>% 
  filter(rci == max(rci))

## potential for growth
Potgrowth <- Data %>% 
  tidyr::pivot_longer(., cols = starts_with("rci"),
                      names_to = "party", names_prefix = "rci_",
                      values_to = "rci") %>% 
  group_by(id, source_id) %>% 
  filter(rci != max(rci) | rci == 0)

## vote intent
Voteint <- Solidity %>%
  # remove undecided voters
  filter(rci != 0)
  
# Train models ------------------------------------------------------------------

## Vote solidity -----------------------------------------------------------

model_CAQ_solidity <- lm(rci ~ age * langue * granular * male, data = Solidity %>% filter(party == "CAQ"))
saveRDS(model_CAQ_solidity, "_SharedFolder_article_pot-growth/data/marts/step6_potgrowth_solidity/models/vote_solidity/model_CAQ.rds")

# For PLQ
model_PLQ_solidity <- lm(rci ~ age * langue * granular * male, data = Solidity %>% filter(party == "PLQ"))
saveRDS(model_PLQ_solidity, "_SharedFolder_article_pot-growth/data/marts/step6_potgrowth_solidity/models/vote_solidity/model_PLQ.rds")

# For QS
model_QS_solidity <- lm(rci ~ age * langue * granular * male, data = Solidity %>% filter(party == "QS"))
saveRDS(model_QS_solidity, "_SharedFolder_article_pot-growth/data/marts/step6_potgrowth_solidity/models/vote_solidity/model_QS.rds")

# For PQ
model_PQ_solidity <- lm(rci ~ age * langue * granular * male, data = Solidity %>% filter(party == "PQ"))
saveRDS(model_PQ_solidity, "_SharedFolder_article_pot-growth/data/marts/step6_potgrowth_solidity/models/vote_solidity/model_PQ.rds")

# For PCQ
model_PCQ_solidity <- lm(rci ~ age * langue * granular * male, data = Solidity %>% filter(party == "PCQ"))
saveRDS(model_PCQ_solidity, "_SharedFolder_article_pot-growth/data/marts/step6_potgrowth_solidity/models/vote_solidity/model_PCQ.rds")


## Potential for growth --------------------------------------------------

model_CAQ_potgrowth <- lm(rci ~ age * langue * granular * male, data = Potgrowth %>% filter(party == "CAQ"))
saveRDS(model_CAQ_potgrowth, "_SharedFolder_article_pot-growth/data/marts/step6_potgrowth_solidity/models/potgrowth/model_CAQ.rds")

# For PLQ
model_PLQ_potgrowth <- lm(rci ~ age * langue * granular * male, data = Potgrowth %>% filter(party == "PLQ"))
saveRDS(model_PLQ_potgrowth, "_SharedFolder_article_pot-growth/data/marts/step6_potgrowth_solidity/models/potgrowth/model_PLQ.rds")

# For QS
model_QS_potgrowth <- lm(rci ~ age * langue * granular * male, data = Potgrowth %>% filter(party == "QS"))
saveRDS(model_QS_potgrowth, "_SharedFolder_article_pot-growth/data/marts/step6_potgrowth_solidity/models/potgrowth/model_QS.rds")

# For PQ
model_PQ_potgrowth <- lm(rci ~ age * langue * granular * male, data = Potgrowth %>% filter(party == "PQ"))
saveRDS(model_PQ_potgrowth, "_SharedFolder_article_pot-growth/data/marts/step6_potgrowth_solidity/models/potgrowth/model_PQ.rds")

# For PCQ
model_PCQ_potgrowth <- lm(rci ~ age * langue * granular * male, data = Potgrowth %>% filter(party == "PCQ"))
saveRDS(model_PCQ_potgrowth, "_SharedFolder_article_pot-growth/data/marts/step6_potgrowth_solidity/models/potgrowth/model_PCQ.rds")


## Vote int --------------------------------------------------------------

model <- nnet::multinom(party ~ age * langue * granular + male, Voteint)
saveRDS(model, "_SharedFolder_article_pot-growth/data/marts/step6_potgrowth_solidity/models/model_voteint.rds")
