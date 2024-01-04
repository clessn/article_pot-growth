# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
Swingness <- readRDS("_SharedFolder_article_pot-growth/data/marts/electoral_swings/provqc2022.rds")

Rci <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2022/aggregated/potgrowth_votesolidity.rds") %>% 
  mutate(## adjust when out of borders
         conf_low = ifelse(conf_low <= -1 & model == "potgrowth", -1, conf_low),
         conf_low = ifelse(conf_low <= 0 & model == "vote_solidity", 0, conf_low),
         conf_high = ifelse(conf_high >= 0 & model == "potgrowth", 0, conf_high),
         conf_high = ifelse(conf_high >= 1 & model == "vote_solidity", 1, conf_high),
         weighted_mean_estimate = ifelse(weighted_mean_estimate <= -1 & model == "potgrowth", -1, weighted_mean_estimate),
         weighted_mean_estimate = ifelse(weighted_mean_estimate >= 0 & model == "potgrowth", 0, weighted_mean_estimate),
         weighted_mean_estimate = ifelse(weighted_mean_estimate >= 1 & model == "vote_solidity", 1, weighted_mean_estimate),
         weighted_mean_estimate = ifelse(weighted_mean_estimate <= 0 & model == "vote_solidity", 0, weighted_mean_estimate))

Ridings <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  select(riding_id, riding_name)

Voteint <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2022/disaggregated/voteint.rds") %>% 
  group_by(riding_id, party) %>% 
  summarise(predicted_vote_share = weighted.mean(x = predicted_vote_share,
                                                 w = prct))

# Join --------------------------------------------------------------------

Data <- left_join(Rci, Swingness, by = c("riding_id", "party")) %>% 
  left_join(., Voteint, by = c("riding_id", "party"))

# Graph -------------------------------------------------------------------

provqc22_colors <- c("CAQ" = "#00cccc","PLQ" = "#FF0024","PQ" = "#099FFF",
                     "QS" = "#FF6600","PCQ"="purple")

ggplot(Data, aes(x = weighted_mean_estimate, y = electoral_swingness)) +
  geom_point() +
  facet_wrap(~model,
             scales = "free_x") +
  geom_smooth(method = "loess") +
  geom_smooth(method = "lm") +
  xlab("IRC moyen estimé")

ggsave("_SharedFolder_article_pot-growth/graphs/step4_compare_rci_swingness/explo/provqc2022/all.png")


## pot growth
Data %>% 
  filter(model == "potgrowth") %>%
  mutate(potential_gains = 1 - predicted_vote_share) %>% 
  ggplot(aes(x = weighted_mean_estimate, y = electoral_swingness)) +
  geom_point(aes(color = party, alpha = potential_gains),
             size = 3) +
  scale_color_manual(values = provqc22_colors) +
  geom_smooth(method = "lm") +
  xlab("IRC moyen estimé des électeurs potentiels") +
  facet_wrap(~party) +
  scale_x_continuous(limits = c(-0.7, -0.35))

ggsave("_SharedFolder_article_pot-growth/graphs/step4_compare_rci_swingness/explo/provqc2022/byparty_potgrowth.png",
       width = 8, height = 6)

## vote solidity
Data %>% 
  filter(model == "vote_solidity") %>%
  ggplot(aes(x = weighted_mean_estimate, y = electoral_swingness)) +
  geom_point(aes(color = party, alpha = predicted_vote_share),
             size = 3) +
  scale_color_manual(values = provqc22_colors) +
  geom_smooth(method = "lm") +
  xlab("IRC moyen estimé des électeurs") +
  facet_wrap(~party, scales = "free_x")

ggsave("_SharedFolder_article_pot-growth/graphs/step4_compare_rci_swingness/explo/provqc2022/byparty_votesolidity.png",
       width = 8, height = 6)
