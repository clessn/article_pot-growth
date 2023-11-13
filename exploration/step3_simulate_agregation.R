# Packages ----------------------------------------------------------------
library(tidyverse)

# Simulation parameters
genders <- c('male', 'female')
age_groups <- c('young', 'middle', 'senior')
languages <- c('French', 'English', 'Other')
districts <- LETTERS[1:10]  # 'A', 'B', 'C', ..., 'J'
# Définition des partis
parties <- c("X", "Y", "Z")

# Vos données de densité et d'intervalles
density <- c(3.04056437, 0.68007055, 0.61069959, 0.48677249, 0.74473839, 0.49664903, 0.52322163, 0.54297472, 0.49994121, 0.65514403, 0.28242210, 0.26, 0.25867137, 0.19870664, 0.15896531, 0.19729571, 0.08700764, 0.08348031, 0.07760141, 0.06043504, 0.31463845)
breaks <- c(-1.0, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)

# Create all combinations of strats
strats <- expand.grid(gender = genders, age = age_groups, lang = languages)

# Simulate data for each strat and district
set.seed(123) # For reproducibility
data <- list()

for (district in districts) {
  for (i in 1:nrow(strats)) {
    strat_data <- strats[i, ]
    N <- sample(100:700, 1)  # Taille de l'échantillon pour cette stratégie
    margin_error <- runif(1, 0.05, 0.2)  # Marge d'erreur générée une fois pour la stratégie
    
    for (party in parties) {
      party_data <- cbind(strat_data, party = party,
                          estimate_irc = sample(x = breaks, size = 1, prob = density),
                          margin_error = margin_error)
      party_data$lower_ci <- pmax(-1, party_data$estimate_irc - party_data$margin_error)
      party_data$upper_ci <- pmin(1, party_data$estimate_irc + party_data$margin_error)
      party_data$district <- district
      party_data$N <- N
      
      data[[paste(district, i, party, sep = "_")]] <- party_data
    }
  }
}

# Combiner les données en un seul DataFrame
data_df <- do.call(rbind, data)

# Ajouter des poids pour chaque stratégie dans chaque district
data <- data_df %>%
  group_by(district, party) %>%
  mutate(weight = N / sum(N)) %>% 
  ungroup() %>% 
  mutate(id = rep(1:(nrow(.)/length(parties)), each = length(parties)))

# Graph 1: by strat by riding -------------------------------------------------------

data %>% 
  filter(district == "A") %>%
  mutate(district = "Assomption") %>% 
  ggplot(aes(x = estimate_irc, y = reorder(interaction(gender, age, lang), estimate_irc),
             group = party, color = party)) + 
  facet_wrap(~district) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_point() +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci))

# Graph 2: agregate by riding ---------------------------------------------

# https://acsdatacommunity.prb.org/discussion-forum/f/forum/292/using-margins-of-error-in-data-aggregation

results <- data %>%
  mutate(se = margin_error / 1.96,
         se2 = se ^ 2) %>% 
  group_by(district, party) %>%
  summarise(
    irc = weighted.mean(x = estimate_irc, w = weight),
    margin_error = sqrt(sum(se2))  # Ceci est une simplification
  ) %>% 
  mutate(lower_ci = irc - margin_error,
         upper_ci = irc + margin_error)

results %>% 
  ggplot(aes(x = irc, y = reorder(district, irc),
             group = party, color = party)) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci),
                position = position_dodge(width = 0.5)) +
  scale_x_continuous(limits = c(-1, 1))


# Simulation --------------------------------------------------------------

## Widen
data_wide <- data %>% 
  pivot_wider(
    id_cols = c(id, gender, age, lang, margin_error, district, N, weight),
    names_from = party, 
    values_from = estimate_irc,
    names_prefix = "rci_"
  )

## We will take each row of data
#### Repeat the individual by his weight * 1000 (so proportion of 1000 respondents from the district)
data_wide$repeats <- round(data_wide$weight * 1000)
## Sample a normal distribution of its RCI for the parties
### So first, need to estimate the standard deviation
data_wide$sd <- data_wide$margin_error / 1.96 ### at 5% level

sim <- function(data){
  for (i in 1:nrow(data)){
    repeatsi <- data$repeats[i]
    sim_resultsi <- data.frame(strat = i,
                               individual = 1:repeatsi,
                               gender = rep(data$gender[i], repeatsi),
                               age = rep(data$age[i], repeatsi),
                               lang = rep(data$lang[i], repeatsi),
                               district = rep(data$district[i], repeatsi))
    for (j in parties){
      sim_resultsi[[paste0("rci_", j)]] <- rnorm(n = repeatsi,
                                                 mean = data[[paste0("rci_", j)]][i],
                                                 sd = data$sd[i])
      }
    if (i == 1){
      sim_results <- sim_resultsi
    } else {
      sim_results <- rbind(sim_results,
                           sim_resultsi)
    }
  }
  return(sim_results)
}


for (i in 1:1000){
  if (i == 1){
    sims <- sim(data_wide) %>% 
      mutate(sim = i)
  } else {
    sims <- sim(data_wide) %>%
      mutate(sim = i) %>% 
      rbind(sims, .)
  }
  print(paste0("Sim: ", i))
}

saveRDS(sims, "_SharedFolder_article_pot-growth/simdata.rds")


# Agregate by riding for each sim ----------------------------------------------------------------

with_vote <- sims %>% 
  pivot_longer(., cols = starts_with("rci"),
               names_prefix = "rci_",
               names_to = "party",
               values_to = "rci") %>%
  group_by(sim, strat, individual) %>% 
  mutate(vote = max(rci)) %>% 
  ungroup() %>% 
  filter(rci == vote)

agg_riding <- with_vote %>% 
  group_by(sim, district, party) %>%
  summarise(nvotes = n()) %>% 
  group_by(sim, district) %>% 
  mutate(total = sum(nvotes),
         prop_vote = nvotes / total,
         seat = ifelse(prop_vote == max(prop_vote), 1, 0))

agg_party <- agg_riding %>% 
  group_by(sim, party) %>% 
  summarise(n = sum(seat))

ggplot(agg_party, aes(x = n, y = party)) +
  ggridges::geom_density_ridges(scale = 0.95)
