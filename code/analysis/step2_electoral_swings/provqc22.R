# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
ElxnResults <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/step2_electoral_swings/results_provqc2022.rds")
Forecast <- readRDS("_SharedFolder_article_pot-growth/data/marts/predictions_by_riding/provqc2022.rds")

## Get relative vote share (ElxnResults) -----------------------------------

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

## Get relative people pred (Forecast) -----------------------------------

## add column rank of party
Forecast$rank_ppred <- pull(Forecast %>%
                           group_by(riding_id, riding_name) %>%
                           mutate(rank_ppred = rank(-people_pred)), rank_ppred)

### create vector of winning vote shares in each riding
winners <- Forecast$people_pred[Forecast$rank_ppred == 1]
### associate with riding id
names(winners) <- Forecast$riding_id[Forecast$rank_ppred == 1]

### create vector of second vote shares in each riding
second <- Forecast$people_pred[Forecast$rank_ppred == 2]
### associate with riding id
names(second) <- Forecast$riding_id[Forecast$rank_ppred == 2]

## add winning and second shares in dataframe
Forecast$winner_ppred <- winners[as.character(Forecast$riding_id)]
Forecast$second_ppred <-  second[as.character(Forecast$riding_id)]

## add relative vote share
### for winners: vote share - second vote share
### for losers: vote share - winning vote share

Forecast$relative_people_pred <- ifelse(Forecast$rank_ppred == 1,
                                          Forecast$people_pred - Forecast$second_ppred,
                                          Forecast$people_pred - Forecast$winner_ppred)

# Exploration -------------------------------------------------------------

graph <- left_join(Forecast, ElxnResults,
                   by = c("riding_id", "riding_name", "party"))

ggplot(graph, aes(x = people_pred, y = relative_vote_share)) +
  geom_point(aes(color = party)) +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0.5, linetype = "dotted")

ggplot(graph, aes(x = people_pred, y = relative_vote_share)) +
  geom_point() +
  facet_wrap(~party) +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0.5, linetype = "dotted")


model <-  lm(relative_vote_share ~ people_pred, data = graph)

graph$pred_model <- predict(object = model, newdata = graph)

graph$electoral_swingness <- graph$relative_vote_share - graph$pred_model

provqc2022_continuous <- graph %>% 
  select(riding_id, riding_name, party, people_pred, prop_vote, relative_vote_share, pred_model, electoral_swingness)

saveRDS(provqc2022_continuous, "_SharedFolder_article_pot-growth/data/marts/electoral_swings/provqc2022_continuous.rds")

# Binary version (is the forecasted winner the real winner) -------------------------

ForecastedWinners <- Forecast %>% 
  filter(rank_ppred == 1) %>% 
  select(riding_id, riding_name,
         forecasted_winner = party,
         forecasted_win_gap = relative_people_pred)

ElxnWinners <- ElxnResults %>% 
  filter(rank == 1) %>% 
  select(riding_id,
         real_winner = party,
         real_win_gap = relative_vote_share)
  
provqc2022 <- left_join(ForecastedWinners, ElxnWinners,
                        by = "riding_id") %>% 
  mutate(surprise_winner = ifelse(forecasted_winner != real_winner, 1, 0))

saveRDS(provqc2022, "_SharedFolder_article_pot-growth/data/marts/electoral_swings/provqc2022_binary.rds")


ggplot(provqc2022, aes(x = forecasted_win_gap, y = real_win_gap)) +
  geom_point(aes(color = factor(surprise_winner))) +
  geom_smooth()
