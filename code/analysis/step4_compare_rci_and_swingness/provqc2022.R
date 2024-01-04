# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
Swingness <- readRDS("_SharedFolder_article_pot-growth/data/marts/electoral_swings/provqc2022_continuous.rds")

Surprises <- readRDS("_SharedFolder_article_pot-growth/data/marts/electoral_swings/provqc2022_binary.rds")

Rci <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2022/aggregated/potgrowth_votesolidity.rds")

Ridings <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  select(riding_id, riding_name)

# Join --------------------------------------------------------------------

Data <- left_join(Rci, Swingness, by = c("riding_id", "party"))

# Graph with continuous swingness -------------------------------------------------------------------

provqc22_colors <- c("CAQ" = "#00cccc","PLQ" = "#FF0024","PQ" = "#099FFF",
                     "QS" = "#FF6600","PCQ"="purple")

ggplot(Data, aes(x = weighted_mean_estimate, y = electoral_swingness)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_smooth(method = "lm") +
  xlab("IRC moyen estimé")

ggsave("_SharedFolder_article_pot-growth/graphs/step4_compare_rci_swingness/explo/provqc2022/all.png")

ggplot(Data, aes(x = weighted_mean_estimate, y = electoral_swingness)) +
  geom_point(aes(color = party),
             show.legend = FALSE) +
  scale_color_manual(values = provqc22_colors) +
  geom_smooth(method = "lm") +
  xlab("IRC moyen estimé") +
  facet_wrap(~party)

ggsave("_SharedFolder_article_pot-growth/graphs/step4_compare_rci_swingness/explo/provqc2022/byparty.png",
       width = 8, height = 6)

# With binary surprise winners --------------------------------------------

ggplot(Surprises, aes(x = forecasted_win_gap, y = real_win_gap)) +
  geom_point(aes(color = factor(surprise_winner)))

long_surprises <- Surprises %>%
  tidyr::pivot_longer(
    cols = c(forecasted_winner, forecasted_win_gap, real_winner, real_win_gap),
    names_to = c("type", ".value"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>% 
  rename(party = winner,
         gap = win) %>% 
  mutate(joinkey = ifelse(type == "forecasted", "vote_solidity", "potgrowth"))

Graph <- Rci %>%
  select(riding_id, party, joinkey = model, weighted_mean_estimate) %>%
  left_join(long_surprises, ., by = c("riding_id", "party",
                                      "joinkey"))
  
ggplot(Graph, aes(x = weighted_mean_estimate, y = gap)) +
  facet_wrap(~ joinkey,
             scales = "free_x") +
  geom_point(aes(color = factor(surprise_winner)))


##### Only potgrowth
GraphPg <- Surprises %>% 
  left_join(., Rci %>% filter(model == "potgrowth"),
            by = c("riding_id", "real_winner" = "party"))

ggplot(GraphPg, aes(x = forecasted_win_gap,
                    y = real_win_gap)) +
  geom_point(aes(color = weighted_mean_estimate,
                 alpha = factor(surprise_winner),
                 shape = factor(surprise_winner)),
             size = 4) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_color_gradient(low = "darkred",
                        high = "green")

##### Only votesol
GraphVs <- Surprises %>% 
  left_join(., Rci %>% filter(model == "vote_solidity"),
            by = c("riding_id", "forecasted_winner" = "party"))

ggplot(GraphVs, aes(x = forecasted_win_gap,
                    y = real_win_gap)) +
  geom_point(aes(color = weighted_mean_estimate,
                 alpha = factor(surprise_winner),
                 shape = factor(surprise_winner)),
             size = 4) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_color_gradient(low = "darkred",
                       high = "green")
