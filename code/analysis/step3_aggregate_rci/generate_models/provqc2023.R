# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/rcis_prov.rds") %>% 
  select(-female) %>% 
  rbind(., readRDS("_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/prov_2023/quorum_mcq_pilote.rds"))
Ridings <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  select(riding_id, large, granular)

# Join riding region on Data
Data <- left_join(Data, Ridings, by = "riding_id") %>% 
  mutate(riding_id = factor(riding_id),
         large = factor(large),
         granular = factor(granular),
         age = factor(age),
         langue = factor(langue))

### Add weights for year (put x more importance on 2023 than 2022 in the regressions)
x <- 3
Data$year_weight <- ifelse(Data$year == 2023, (x * table(Data$year)["2022"]) / table(Data$year)["2023"], 1)

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

model_CAQ_solidity <- lm(rci ~ age * langue * granular * male, data = Solidity %>% filter(party == "CAQ"),
                         weights = year_weight)
saveRDS(model_CAQ_solidity, "_SharedFolder_article_pot-growth/data/marts/models/provqc2023/vote_solidity/model_CAQ.rds")

# For PLQ
model_PLQ_solidity <- lm(rci ~ age * langue * granular * male, data = Solidity %>% filter(party == "PLQ"),
                         weights = year_weight)
saveRDS(model_PLQ_solidity, "_SharedFolder_article_pot-growth/data/marts/models/provqc2023/vote_solidity/model_PLQ.rds")

# For QS
model_QS_solidity <- lm(rci ~ age * langue * granular * male, data = Solidity %>% filter(party == "QS"),
                        weights = year_weight)
saveRDS(model_QS_solidity, "_SharedFolder_article_pot-growth/data/marts/models/provqc2023/vote_solidity/model_QS.rds")

# For PQ
model_PQ_solidity <- lm(rci ~ age * langue * granular * male, data = Solidity %>% filter(party == "PQ"),
                        weights = year_weight)
saveRDS(model_PQ_solidity, "_SharedFolder_article_pot-growth/data/marts/models/provqc2023/vote_solidity/model_PQ.rds")

# For PCQ
model_PCQ_solidity <- lm(rci ~ age * langue * granular * male, data = Solidity %>% filter(party == "PCQ"),
                         weights = year_weight)
saveRDS(model_PCQ_solidity, "_SharedFolder_article_pot-growth/data/marts/models/provqc2023/vote_solidity/model_PCQ.rds")


## Potential for growth --------------------------------------------------

model_CAQ_potgrowth <- lm(rci ~ age * langue * granular * male, data = Potgrowth %>% filter(party == "CAQ"),
                          weights = year_weight)
saveRDS(model_CAQ_potgrowth, "_SharedFolder_article_pot-growth/data/marts/models/provqc2023/potgrowth/model_CAQ.rds")

# For PLQ
model_PLQ_potgrowth <- lm(rci ~ age * langue * granular * male, data = Potgrowth %>% filter(party == "PLQ"),
                          weights = year_weight)
saveRDS(model_PLQ_potgrowth, "_SharedFolder_article_pot-growth/data/marts/models/provqc2023/potgrowth/model_PLQ.rds")

# For QS
model_QS_potgrowth <- lm(rci ~ age * langue * granular * male, data = Potgrowth %>% filter(party == "QS"),
                         weights = year_weight)
saveRDS(model_QS_potgrowth, "_SharedFolder_article_pot-growth/data/marts/models/provqc2023/potgrowth/model_QS.rds")

# For PQ
model_PQ_potgrowth <- lm(rci ~ age * langue * granular * male, data = Potgrowth %>% filter(party == "PQ"),
                         weights = year_weight)
saveRDS(model_PQ_potgrowth, "_SharedFolder_article_pot-growth/data/marts/models/provqc2023/potgrowth/model_PQ.rds")

# For PCQ
model_PCQ_potgrowth <- lm(rci ~ age * langue * granular * male, data = Potgrowth %>% filter(party == "PCQ"),
                          weights = year_weight)
saveRDS(model_PCQ_potgrowth, "_SharedFolder_article_pot-growth/data/marts/models/provqc2023/potgrowth/model_PCQ.rds")


## Vote int --------------------------------------------------------------

model <- nnet::multinom(party ~ age * langue * granular + male, Voteint,
                        weights = year_weight)
saveRDS(model, "_SharedFolder_article_pot-growth/data/marts/models/provqc2023/voteint/model.rds")
