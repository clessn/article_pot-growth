# Packages ----------------------------------------------------------------
library(dplyr)


# Convertir scores en probs -----------------------------------------------

## 1. (1 + x)/sum(1 + x)

# https://math.stackexchange.com/questions/2891778/how-to-convert-an-array-of-numbers-into-probability-values
# https://en.wikipedia.org/wiki/Softmax_function
# https://en.wikipedia.org/wiki/Propagation_of_uncertainty

## Faire simulation fixe, seulement un lancer de dés
## Puis aussi faire une simulation en inputant un indicateur de comm pol

# Functions ---------------------------------------------------------------

# Fonction sigmoïde pour convertir des scores en probabilités
sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

# Data --------------------------------------------------------------------

Data <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2022/disaggregated.rds")

oneprofile <- Data %>% 
  filter(riding_id == 326 &
           male == 1 &
           langue == "english" &
           age == "55p")

num_simulations <- 1000

# Calcul des probabilités
party_probabilities <- sapply(1:nrow(oneprofile), function(i) {
  simulated_scores <- rnorm(n = 1000, mean = oneprofile$estimate[i],
                            sd = oneprofile$std.error[i])
  mean(sigmoid(simulated_scores))
})

names(party_probabilities) <- oneprofile$party

party_probabilities / sum(party_probabilities)
