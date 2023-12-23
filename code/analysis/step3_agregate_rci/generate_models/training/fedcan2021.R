# Packages ----------------------------------------------------------------
library(dplyr)

source("code/analysis/step3_agregate_rci/generate_models/functions.R",
       encoding = "UTF-8")

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/rcis_fed.rds")
Ridings <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  select(-level, -riding_name) %>% 
  mutate(riding_id = as.character(riding_id))

# Join riding region on Data
Data <- left_join(Data, Ridings, by = "riding_id") %>% 
  mutate(riding_id = factor(riding_id),
         prov_terr = factor(prov_terr),
         large = factor(large),
         density = factor(density),
         granular = factor(granular),
         age = factor(age),
         langue = factor(langue)) %>% 
  tidyr::drop_na(starts_with("rci"), age, langue, male, large,
                 prov_terr, density, granular)

parties <- c("PLC", "PCC", "NPD", "BQ", "PVC")

# Train/test --------------------------------------------------------------

set.seed(123)
test_indices <- caret::createDataPartition(Data$id, p = 0.2, list = FALSE, times = 1)
# Séparer les données en ensembles de test et d'entraînement
test_data <- Data[test_indices, ]
train_data <- Data[-test_indices, ]

saveRDS(test_data, "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/test_data.rds")

# Models ------------------------------------------------------------------

## Linear ------------------------------------------------------------------

### Lineaire simple ---------------------------------------------------------

# Modèle simple avec provterr
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/linear",
                model_name = "simple_provterr",
                right_equation = "age + langue + male + prov_terr",
                parties = parties,
                mix_model = FALSE)

# Modèle simple avec large
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/linear",
                model_name = "simple_large",
                right_equation = "age + langue + male + large",
                parties = parties,
                mix_model = FALSE)

# Modèle simple avec provterr*density
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/linear",
                model_name = "simple_provterrXdensity",
                right_equation = "age + langue + male + prov_terr * density",
                parties = parties,
                mix_model = FALSE)

# Modèle simple avec large*density
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/linear",
                model_name = "simple_largeXdensity",
                right_equation = "age + langue + male + large * density",
                parties = parties,
                mix_model = FALSE)

# Modèle simple avec granular
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/linear",
                model_name = "simple_granular",
                right_equation = "age + langue + male + granular",
                parties = parties,
                mix_model = FALSE)

### Interaction age-langue --------------------------------------------------

# Interaction age et langue avec provterr
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/linear",
                model_name = "interaction_ageXlangue_provterr",
                right_equation = "age * langue + male + prov_terr",
                parties = parties,
                mix_model = FALSE)

# Interaction age et langue avec large
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/linear",
                model_name = "interaction_ageXlangue_large",
                right_equation = "age * langue + male + large",
                parties = parties,
                mix_model = FALSE)

# Interaction age et langue avec provterr*density
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/linear",
                model_name = "interaction_ageXlangue_provterrXdensity",
                right_equation = "age * langue + male + prov_terr * density",
                parties = parties,
                mix_model = FALSE)

# Interaction age et langue avec large*density
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/linear",
                model_name = "interaction_ageXlangue_largeXdensity",
                right_equation = "age * langue + male + large * density",
                parties = parties,
                mix_model = FALSE)

# Interaction age et langue avec granular
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/linear",
                model_name = "interaction_ageXlangue_granular",
                right_equation = "age * langue + male + granular",
                parties = parties,
                mix_model = FALSE)

### Interaction age-langue-region -------------------------------------------

# Interaction age et langue avec provterr
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/linear",
                model_name = "interaction_ageXlangueXprovterr",
                right_equation = "age * langue * prov_terr + male",
                parties = parties,
                mix_model = FALSE)

# Interaction age et langue avec large
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/linear",
                model_name = "interaction_ageXlangueXlarge",
                right_equation = "age * langue * large + male",
                parties = parties,
                mix_model = FALSE)

# Interaction age et langue avec provterr et density
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/linear",
                model_name = "interaction_ageXlangueXprovterrXdensity",
                right_equation = "age * langue * prov_terr * density + male",
                parties = parties,
                mix_model = FALSE)

# Interaction age et langue avec large et density
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/linear",
                model_name = "interaction_ageXlangueXlargeXdensity",
                right_equation = "age * langue * large * density + male",
                parties = parties,
                mix_model = FALSE)

# Interaction age et langue avec granular
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/linear",
                model_name = "interaction_ageXlangueXgranular",
                right_equation = "age * langue * granular + male",
                parties = parties,
                mix_model = FALSE)


## Mixed -------------------------------------------------------------------

### Prov terr ------------------------------------------------------------

# Effets aléatoires simples pour prov_terr
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_provterr_simple",
                right_equation = "age + langue + male + (1 | prov_terr)",
                parties = parties,
                mix_model = TRUE)

# Effets aléatoires pour age dans prov_terr
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_provterr_age",
                right_equation = "age + langue + male + (age | prov_terr)",
                parties = parties,
                mix_model = TRUE)

# Effets aléatoires pour langue dans prov_terr
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_provterr_langue",
                right_equation = "age + langue + male + (langue | prov_terr)",
                parties = parties,
                mix_model = TRUE)

# Effets aléatoires combinés pour age et langue dans prov_terr
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_provterr_age_langue",
                right_equation = "age + langue + male + (age + langue | prov_terr)",
                parties = parties,
                mix_model = TRUE)

# Interaction entre age et langue avec effets aléatoires dans prov_terr
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_provterr_ageXlangue",
                right_equation = "age + langue + male + (age * langue | prov_terr)",
                parties = parties,
                mix_model = TRUE)


### Large ------------------------------------------------------------

# Effets aléatoires simples pour large
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_large_simple",
                right_equation = "age + langue + male + (1 | large)",
                parties = parties,
                mix_model = TRUE)

# Effets aléatoires pour age dans large
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_large_age",
                right_equation = "age + langue + male + (age | large)",
                parties = parties,
                mix_model = TRUE)

# Effets aléatoires pour langue dans large
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_large_langue",
                right_equation = "age + langue + male + (langue | large)",
                parties = parties,
                mix_model = TRUE)

# Effets aléatoires combinés pour age et langue dans large
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_large_age_langue",
                right_equation = "age + langue + male + (age + langue | large)",
                parties = parties,
                mix_model = TRUE)

# Interaction entre age et langue avec effets aléatoires dans large
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_large_ageXlangue",
                right_equation = "age + langue + male + (age * langue | large)",
                parties = parties,
                mix_model = TRUE)

### Prov terr * density ------------------------------------------------------------

# Effets aléatoires simples pour prov_terr * density
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_provterr_density_simple",
                right_equation = "age + langue + male + (1 | prov_terr:density)",
                parties = parties,
                mix_model = TRUE)

# Effets aléatoires pour age dans prov_terr * density
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_provterr_density_age",
                right_equation = "age + langue + male + (age | prov_terr:density)",
                parties = parties,
                mix_model = TRUE)

# Effets aléatoires pour langue dans prov_terr * density
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_provterr_density_langue",
                right_equation = "age + langue + male + (langue | prov_terr:density)",
                parties = parties,
                mix_model = TRUE)

# Effets aléatoires combinés pour age et langue dans prov_terr * density
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_provterr_density_age_langue",
                right_equation = "age + langue + male + (age + langue | prov_terr:density)",
                parties = parties,
                mix_model = TRUE)

# Interaction entre age et langue avec effets aléatoires dans prov_terr * density
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_provterr_density_ageXlangue",
                right_equation = "age + langue + male + (age * langue | prov_terr:density)",
                parties = parties,
                mix_model = TRUE)

### Large * density ------------------------------------------------------------

# Effets aléatoires simples pour large * density
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_large_density_simple",
                right_equation = "age + langue + male + (1 | large:density)",
                parties = parties,
                mix_model = TRUE)

# Effets aléatoires pour age dans large * density
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_large_density_age",
                right_equation = "age + langue + male + (age | large:density)",
                parties = parties,
                mix_model = TRUE)

# Effets aléatoires pour langue dans large * density
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_large_density_langue",
                right_equation = "age + langue + male + (langue | large:density)",
                parties = parties,
                mix_model = TRUE)

# Effets aléatoires combinés pour age et langue dans large * density
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_large_density_age_langue",
                right_equation = "age + langue + male + (age + langue | large:density)",
                parties = parties,
                mix_model = TRUE)

# Interaction entre age et langue avec effets aléatoires dans large * density
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_large_density_ageXlangue",
                right_equation = "age + langue + male + (age * langue | large:density)",
                parties = parties,
                mix_model = TRUE)

### Granular ------------------------------------------------------------

# Effets aléatoires simples pour granular
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_granular_simple",
                right_equation = "age + langue + male + (1 | granular)",
                parties = parties,
                mix_model = TRUE)

# Effets aléatoires pour age dans granular
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_granular_age",
                right_equation = "age + langue + male + (age | granular)",
                parties = parties,
                mix_model = TRUE)

# Effets aléatoires pour langue dans granular
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_granular_langue",
                right_equation = "age + langue + male + (langue | granular)",
                parties = parties,
                mix_model = TRUE)

# Effets aléatoires combinés pour age et langue dans granular
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_granular_age_langue",
                right_equation = "age + langue + male + (age + langue | granular)",
                parties = parties,
                mix_model = TRUE)

# Interaction entre age et langue avec effets aléatoires dans granular
save_model_list(data = train_data,
                path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files/mixed",
                model_name = "mixed_granular_ageXlangue",
                right_equation = "age + langue + male + (age * langue | granular)",
                parties = parties,
                mix_model = TRUE)

