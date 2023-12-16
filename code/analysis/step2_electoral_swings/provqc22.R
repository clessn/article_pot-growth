# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
ElxnResults <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/step2_electoral_swings/results_provqc2022.rds")
Forecast <- readRDS("_SharedFolder_article_pot-growth/data/marts/predictions_by_riding/provqc2022.rds")

## add column rank of party 
ElxnResults$rank <- pull(ElxnResults %>%
                           group_by(riding_id, riding_name) %>%
                           mutate(rank = rank(-prop_vote)), rank)

### create vector of winning vote shares in each riding
winners <- ElxnResults$prop_vote[ElxnResults$rank == 1]
### associate with riding id
names(winners) <- ElxnResults$riding_id[ElxnResults$rank == 1]

### create vector of second vote shares in each riding
second <- ElxnResults$prop_vote[ElxnResults$rank == 2]
### associate with riding id
names(second) <- ElxnResults$riding_id[ElxnResults$rank == 2]

## add winning and second shares in dataframe
ElxnResults$winner <- winners[as.character(ElxnResults$riding_id)]
ElxnResults$second <-  second[as.character(ElxnResults$riding_id)]

## add relative vote share
### for winners: vote share - second vote share
### for losers: vote share - winning vote share

ElxnResults$relative_vote_share <- ifelse(ElxnResults$rank == 1,
                                          ElxnResults$prop_vote - ElxnResults$second,
                                          ElxnResults$prop_vote - ElxnResults$winner)

# Exploration -------------------------------------------------------------

graph <- left_join(Forecast, ElxnResults,
                   by = c("riding_id", "riding_name", "party"))

ggplot(graph, aes(x = prop_forecasted, y = relative_vote_share)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0.5, linetype = "dotted")

ggplot(graph, aes(x = prop_forecasted, y = relative_vote_share)) +
  geom_point() +
  facet_wrap(~party) +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0.5, linetype = "dotted")
