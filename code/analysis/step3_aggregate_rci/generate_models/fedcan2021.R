# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/rcis_fed.rds")
Ridings <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  select(riding_id, prov_terr, granular, large, density) %>% 
  mutate(riding_id = as.character(riding_id),
         large_density = paste0(large, "_", density))

# Join riding region on Data
Data <- left_join(Data, Ridings, by = "riding_id") %>% 
  mutate(riding_id = factor(riding_id),
         granular = factor(granular),
         age = factor(age),
         langue = factor(langue)) %>% 
  tidyr::drop_na(starts_with("rci"), age, langue, male, granular)

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

# For PLC
model_PLC_solidity <- lm(rci ~ age * langue * large_density * male, data = Solidity %>% filter(party == "PLC"))
saveRDS(model_PLC_solidity, "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/vote_solidity/model_PLC.rds")

# For PCC
model_PCC_solidity <- lm(rci ~ age * langue * large_density * male, data = Solidity %>% filter(party == "PCC"))
saveRDS(model_PCC_solidity, "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/vote_solidity/model_PCC.rds")

# For NPD
model_NPD_solidity <- lm(rci ~ age * langue * large_density * male, data = Solidity %>% filter(party == "NPD"))
saveRDS(model_NPD_solidity, "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/vote_solidity/model_NPD.rds")

# For BQ
model_BQ_solidity <- lm(rci ~ age * langue * large_density * male, data = Solidity %>% filter(party == "BQ"))
saveRDS(model_BQ_solidity, "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/vote_solidity/model_BQ.rds")

# For PVC
model_PVC_solidity <- lm(rci ~ age * langue * large_density * male, data = Solidity %>% filter(party == "PVC"))
saveRDS(model_PVC_solidity, "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/vote_solidity/model_PVC.rds")


## Potential for growth ----------------------------------------------------

# For PLC
model_PLC_potgrowth <- lm(rci ~ age * langue * large_density * male, data = Potgrowth %>% filter(party == "PLC"))
saveRDS(model_PLC_potgrowth, "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/potgrowth/model_PLC.rds")

# For PCC
model_PCC_potgrowth <- lm(rci ~ age * langue * large_density * male, data = Potgrowth %>% filter(party == "PCC"))
saveRDS(model_PCC_potgrowth, "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/potgrowth/model_PCC.rds")

# For NPD
model_NPD_potgrowth <- lm(rci ~ age * langue * large_density * male, data = Potgrowth %>% filter(party == "NPD"))
saveRDS(model_NPD_potgrowth, "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/potgrowth/model_NPD.rds")

# For BQ
model_BQ_potgrowth <- lm(rci ~ age * langue * large_density * male, data = Potgrowth %>% filter(party == "BQ"))
saveRDS(model_BQ_potgrowth, "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/potgrowth/model_BQ.rds")

# For PVC
model_PVC_potgrowth <- lm(rci ~ age * langue * large_density * male, data = Potgrowth %>% filter(party == "PVC"))
saveRDS(model_PVC_potgrowth, "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/potgrowth/model_PVC.rds")


## Vote int ----------------------------------------------------------------

model <- nnet::multinom(party ~ age * langue * large_density + male, Voteint) ## using large instead of granular to reduce nb of parameters
saveRDS(model, "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/voteint/model.rds")

