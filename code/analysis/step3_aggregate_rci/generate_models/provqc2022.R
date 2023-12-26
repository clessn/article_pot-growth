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


# Train models ------------------------------------------------------------------

model_CAQ <- lm(rci_CAQ ~ age * langue * granular + male, data = Data)
saveRDS(model_CAQ, "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/model_CAQ.rds")

# Ajuster le modèle pour le PLQ et le sauvegarder
model_PLQ <- lm(rci_PLQ ~ age * langue * granular + male, data = Data)
saveRDS(model_PLQ, "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/model_PLQ.rds")

# Ajuster le modèle pour le QS et le sauvegarder
model_QS <- lm(rci_QS ~ age * langue * granular + male, data = Data)
saveRDS(model_QS, "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/model_QS.rds")

# Ajuster le modèle pour le PQ et le sauvegarder
model_PQ <- lm(rci_PQ ~ age * langue * granular + male, data = Data)
saveRDS(model_PQ, "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/model_PQ.rds")

# Ajuster le modèle pour le PCQ et le sauvegarder
model_PCQ <- lm(rci_PCQ ~ age * langue * granular + male, data = Data)
saveRDS(model_PCQ, "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/model_PCQ.rds")

