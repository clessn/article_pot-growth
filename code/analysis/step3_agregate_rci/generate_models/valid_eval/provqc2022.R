# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

source("code/analysis/step3_agregate_rci/generate_models/functions.R",
       encoding = "UTF-8")

# Data --------------------------------------------------------------------
test_data <- readRDS("_SharedFolder_article_pot-growth/data/marts/models/provqc2022/test_data.rds") %>% 
  tidyr::drop_na(starts_with("rci"), age, langue, male, granular, large)

# Loop DONT RUN --------------------------------------------------------------------

### read rds file instead

#files <- list.files(path = "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/model_files",
#                    recursive = TRUE, full.names = TRUE)

#for (i in files){
#  if (i == files[1]){
#    results <- evaluate_models_file(i, test_data)
#  } else {
#    results <- rbind(results, evaluate_models_file(i, test_data)) 
#  }
#  print(i)
#}

#saveRDS(results, "_SharedFolder_article_pot-growth/data/marts/models/provqc2022/diagnostics.rds")

results <- readRDS("_SharedFolder_article_pot-growth/data/marts/models/provqc2022/diagnostics.rds")

# Fonction pour normaliser (inverser si nécessaire)
normalize <- function(x, invert = FALSE) {
  if (invert) { 1 / (x - min(x) + 1) } else { (x - min(x)) / (max(x) - min(x)) }
}

# Application de la normalisation
results$normalized_r_squared <- normalize(results$r_squared)
results$normalized_adj_r_squared <- normalize(results$adj_marginal_r2)
results$normalized_mse <- normalize(results$mse, invert = TRUE)
results$normalized_aic <- normalize(results$aic, invert = TRUE)
results$normalized_bic <- normalize(results$bic, invert = TRUE)

# PCA ---------------------------------------------------------------------

pca <- FactoMineR::PCA(results %>% select(starts_with("normalized")))
pca$eig
pca$var$coord

results$dim1 <- pca$ind$coord[, 1]
results$dim2 <- pca$ind$coord[, 2]


# Find best model by party ------------------------------------------------

results %>% 
  filter(party == "CAQ") %>%
  ggplot(aes(x = dim1, y = dim2)) +
  ggrepel::geom_text_repel(aes(label = model_name),
                           max.overlaps = 100)

results %>% 
  filter(party == "PLQ") %>%
  ggplot(aes(x = dim1, y = dim2)) +
  ggrepel::geom_text_repel(aes(label = model_name),
                           max.overlaps = 100)

results %>% 
  filter(party == "QS") %>%
  ggplot(aes(x = dim1, y = dim2)) +
  ggrepel::geom_text_repel(aes(label = model_name),
                           max.overlaps = 100)

results %>% 
  filter(party == "PQ") %>%
  ggplot(aes(x = dim1, y = dim2)) +
  ggrepel::geom_text_repel(aes(label = model_name),
                           max.overlaps = 100)

results %>% 
  filter(party == "PCQ") %>%
  ggplot(aes(x = dim1, y = dim2)) +
  ggrepel::geom_text_repel(aes(label = model_name),
                           max.overlaps = 100)


### Except for PQ, the best models were intregion_ridingid
##### Best for PQ is intregion_granular. It is also a good one for other parties.

model <- readRDS("_SharedFolder_article_pot-growth/data/marts/models/provqc2022/model_files/linear/intregion_ridingid.rds")$CAQ
model$terms[[3]]

model <- readRDS("_SharedFolder_article_pot-growth/data/marts/models/provqc2022/model_files/linear/intregion_granular.rds")$CAQ
model$terms[[3]]

### Since we have ridings with a small number of respondents, the granular model with small regions instead of ridings will be preferred
