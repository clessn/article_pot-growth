# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/rcis_fed.rds")
Ridings <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  select(riding_id, granular) %>% 
  mutate(riding_id = as.character(riding_id))

# Join riding region on Data
Data <- left_join(Data, Ridings, by = "riding_id") %>% 
  mutate(riding_id = factor(riding_id),
         granular = factor(granular),
         age = factor(age),
         langue = factor(langue)) %>% 
  tidyr::drop_na(starts_with("rci"), age, langue, male, granular)


# Train models ------------------------------------------------------------------

# Ajuster le modèle pour le PLC et le sauvegarder
model_PLC <- lm(rci_PLC ~ age * langue * granular + male, data = Data)
saveRDS(model_PLC, "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_PLC.rds")

# Ajuster le modèle pour le PCC et le sauvegarder
model_PCC <- lm(rci_PCC ~ age * langue * granular + male, data = Data)
saveRDS(model_PCC, "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_PCC.rds")

# Ajuster le modèle pour le NPD et le sauvegarder
model_NPD <- lm(rci_NPD ~ age * langue * granular + male, data = Data)
saveRDS(model_NPD, "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_NPD.rds")

# Ajuster le modèle pour le BQ et le sauvegarder
model_BQ <- lm(rci_BQ ~ age * langue * granular + male, data = Data)
saveRDS(model_BQ, "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_BQ.rds")

# Ajuster le modèle pour le PVC et le sauvegarder
model_PVC <- lm(rci_PVC ~ age * langue * granular + male, data = Data)
saveRDS(model_PVC, "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_PVC.rds")


