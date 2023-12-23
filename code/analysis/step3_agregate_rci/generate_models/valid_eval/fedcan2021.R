# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

source("code/analysis/step3_agregate_rci/generate_models/functions.R",
       encoding = "UTF-8")

# Data --------------------------------------------------------------------
test_data <- readRDS("_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/test_data.rds") %>% 
  tidyr::drop_na(starts_with("rci"), age, langue, male, granular, large, prov_terr, density)

# Loop DONT RUN --------------------------------------------------------------------

### read rds file instead

#files <- list.files(path = "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/model_files",
#                    recursive = TRUE, full.names = TRUE)
#
#for (i in files){
#  if (i == files[1]){
#    results <- evaluate_models_file(i, test_data)
#  } else {
#    results <- rbind(results, evaluate_models_file(i, test_data)) 
#  }
#  print(i)
#}
#
#saveRDS(results, "_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/diagnostics.rds")

results <- readRDS("_SharedFolder_article_pot-growth/data/marts/models/fedcan2021/diagnostics.rds")

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
  filter(party == "PLC") %>%
  ggplot(aes(x = dim1, y = dim2)) +
  ggrepel::geom_text_repel(aes(label = equation),
                           max.overlaps = 100)

results %>% 
  filter(party == "PCC") %>%
  ggplot(aes(x = dim1, y = dim2)) +
  ggrepel::geom_text_repel(aes(label = equation),
                           max.overlaps = 100)

results %>% 
  filter(party == "NPD") %>%
  ggplot(aes(x = dim1, y = dim2)) +
  ggrepel::geom_text_repel(aes(label = equation),
                           max.overlaps = 100)

results %>% 
  filter(party == "BQ") %>%
  ggplot(aes(x = dim1, y = dim2)) +
  ggrepel::geom_text_repel(aes(label = equation),
                           max.overlaps = 100)

results %>% 
  filter(party == "PVC") %>%
  ggplot(aes(x = dim1, y = dim2)) +
  ggrepel::geom_text_repel(aes(label = equation),
                           max.overlaps = 100)


### age * langue * granular look to be the best models