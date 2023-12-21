# Packages ----------------------------------------------------------------
library(dplyr)

source("code/analysis/step3_agregate_rci/generate_models/functions.R",
       encoding = "UTF-8")

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/rcis_prov.rds")
Ridings <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  select(riding_id, large, granular)

# Join riding region on Data
Data <- left_join(Data, Ridings, by = "riding_id") %>% 
  mutate(riding_id = factor(riding_id))

parties <- c("CAQ", "PLQ", "QS", "PQ", "PCQ")


# Models ------------------------------------------------------------------

# Modeles lineaires simples -----------------------------------------------

## riding id
save_model_list(data = Data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/linear",
                model_name = "simple_ridingid",
                right_equation = "age + langue + male + riding_id",
                parties = parties,
                mix_model = FALSE)

## granular
save_model_list(data = Data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/linear",
                model_name = "simple_granular",
                right_equation = "age + langue + male + granular",
                parties = parties,
                mix_model = FALSE)

## large region
save_model_list(data = Data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/linear",
                model_name = "simple_large",
                right_equation = "age + langue + male + large",
                parties = parties,
                mix_model = FALSE)



# Modele lineaires avec interactions sur age et langue -----------------------------

## riding id
save_model_list(data = Data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/linear",
                model_name = "someint_ridingid",
                right_equation = "age * langue + male + riding_id",
                parties = parties,
                mix_model = FALSE)


## granular
save_model_list(data = Data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/linear",
                model_name = "someint_granular",
                right_equation = "age * langue + male + granular",
                parties = parties,
                mix_model = FALSE)

## large
save_model_list(data = Data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/linear",
                model_name = "someint_large",
                right_equation = "age * langue + male + large",
                parties = parties,
                mix_model = FALSE)


# Modele lineaires avec interactions sur age et langue et region -----------------------------

## riding id
#save_model_list(data = Data,
#                path = "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/linear",
#                model_name = "intregion_ridingid",
#                right_equation = "age * langue * riding_id + male",
#                parties = parties,
#                mix_model = FALSE)
#

## granular
save_model_list(data = Data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/linear",
                model_name = "intregion_granular",
                right_equation = "age * langue * granular + male",
                parties = parties,
                mix_model = FALSE)

## large
save_model_list(data = Data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/linear",
                model_name = "intregion_large",
                right_equation = "age * langue * large + male",
                parties = parties,
                mix_model = FALSE)


# Modeles mixtes ----------------------------------------------------------

# Modèle Mixte avec Effet de Groupe sur Granulaire
save_model_list(data = Data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/mixed",
                model_name = "mixed_granular_group",
                right_equation = "age + langue + male + (1 | granular)",
                parties = parties,
                mix_model = TRUE)

# Modèle Mixte avec Interaction Age-Langue et Effet de Groupe sur Granulaire
save_model_list(data = Data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/mixed",
                model_name = "mixed_ageXlangue_granular_group",
                right_equation = "age * langue + male + (1 | granular)",
                parties = parties,
                mix_model = TRUE)

# Modèle Mixte avec Pentes Variables pour Age et Langue par Granulaire
save_model_list(data = Data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/mixed",
                model_name = "mixed_age_langue_by_granular",
                right_equation = "age + langue + male + (age + langue | granular)",
                parties = parties,
                mix_model = TRUE)

# Modèle Mixte avec Pentes Variables pour Age et Langue par Large
save_model_list(data = Data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/mixed",
                model_name = "mixed_age_langue_by_large",
                right_equation = "age + male + langue + (age + langue | large)",
                parties = parties,
                mix_model = TRUE)

# Modèle Mixte avec Pentes Variables pour Age*Langue par Granulaire
save_model_list(data = Data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/mixed",
                model_name = "mixed_ageXlangue_by_granular",
                right_equation = "male + (age * langue | granular)",
                parties = parties,
                mix_model = TRUE)

# Modèle Mixte avec Pentes Variables pour Age*Langue par Large
save_model_list(data = Data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/mixed",
                model_name = "mixed_ageXlangue_by_large",
                right_equation = "male + (age * langue | large)",
                parties = parties,
                mix_model = TRUE)

