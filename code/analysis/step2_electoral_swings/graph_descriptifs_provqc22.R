library(ggplot2)
library(dplyr)
install.packages("ggridges")
library(ggridges)


# Data loading ------------------------------------------------------------

provqc22 <- readRDS("_SharedFolder_article_pot-growth/data/marts/electoral_swings/provqc2022.rds")

# Data wrangling ----------------------------------------------------------

graphCAQ <- provqc22 %>% 
  filter(party == "CAQ")

graphPLQ <- provqc22 %>% 
  filter(party == "PLQ")

graphPQ <- provqc22 %>% 
  filter(party == "PQ")

graphQS <- provqc22 %>% 
  filter(party =="QS")

graphPCQ <- provqc22 %>% 
  filter(party == "PCQ")

# Graph -------------------------------------------------------------------

ggplot(provqc22, aes(x = electoral_swingness)) +
  geom_density(size = 1, color = "red", fill = "red", alpha = 0.8) +
  geom_vline(xintercept = 0) +
  clessnverse::theme_clean_light() +
  labs(title = "Electoral swingness distribution", x = "electoral swingness", y = "")

# Graph CAQ

ggplot(graphCAQ, aes(x = electoral_swingness)) + 
  geom_density(size = 1, color = "#17BBF2", fill = "#17BBF2", alpha = 0.8) +
  geom_vline(xintercept = 0) +
  clessnverse::theme_clean_light() +
  labs(x = "electoral swingness", y = "", title = "Electoral swingness distribution, CAQ") +
  xlim(-0.30, 0.30)

# Graph PLQ

ggplot(graphPLQ, aes(x = electoral_swingness)) + 
  geom_density(size = 1, color = "#ED1A2D", fill = "#ED1A2D", alpha = 0.8) +
  geom_vline(xintercept = 0) +
  clessnverse::theme_clean_light() +
  labs(x = "electoral swingness", y = "", title = "Electoral swingness distribution, LPQ") +
  xlim(-0.30, 0.30)

# Graph PQ

ggplot(graphPQ, aes(x = electoral_swingness)) + 
  geom_density(size = 1, color = "#042C4B", fill = "#042C4B", alpha = 0.8) +
  geom_vline(xintercept = 0) +
  clessnverse::theme_clean_light() +
  labs(x = "electoral swingness", y = "", title = "Electoral swingness distribution, PQ") +
  xlim(-0.30, 0.30)

# Graph QS

ggplot(graphQS, aes(x = electoral_swingness)) + 
  geom_density(size = 1, color = "#FF5605", fill = "#FF5605", alpha = 0.8) +
  geom_vline(xintercept = 0) +
  clessnverse::theme_clean_light() +
  labs(x = "electoral swingness", y = "", title = "Electoral swingness distribution, QS") +
  xlim(-0.30, 0.30)

# Graph PCQ

ggplot(graphPCQ, aes(x = electoral_swingness)) + 
  geom_density(size = 1, color = "#172853", fill = "#172853", alpha = 0.8) +
  geom_vline(xintercept = 0) +
  clessnverse::theme_clean_light() +
  labs(x = "electoral swingness", y = "", title = "Electoral swingness distribution, PCQ") +
  xlim(-0.30, 0.30)

# Graph commun

colors <- c("CAQ" = "#17BBF2", "PLQ" = "#ED1A2D", "PQ" = "#042C4B", "QS" = "#FF5605", "PCQ" = "#172853")

ggplot(provqc22, aes(x = electoral_swingness, color = NA, fill = party, alpha = 0.8)) +
  geom_density() +
  facet_wrap(~party) +
  scale_color_manual(values = NA) +
  scale_fill_manual(values = colors) +
  clessnverse::theme_clean_light() +
  labs(title = "Electoral swingness distribution, by party", 
       x = "electoral swingness", 
       y = "") +
  geom_vline(xintercept = 0) +
  xlim(-0.30, 0.30) +
  theme(legend.position = "none")


# Save --------------------------------------------------------------------

ggsave("_SharedFolder_article_pot-growth/graphs/step2_electoral_swings/provqc22/electoral_swings_distribution.png", height = 10, width = 12)

# Graph ggridges

ggplot(provqc22, aes(x = electoral_swingness, y = party, color = NA, fill = party, alpha = 0.8)) +
  geom_density_ridges()+
  scale_color_manual(values = NA) +
  scale_fill_manual(values = colors) +
  clessnverse::theme_clean_light() +
  labs(title = "Electoral swingness distribution, by party", 
       x = "electoral swingness", 
       y = "") +
  geom_vline(xintercept = 0) +
  xlim(-0.30, 0.30) +
  theme(legend.position = "none")


# Save --------------------------------------------------------------------

ggsave("_SharedFolder_article_pot-growth/graphs/step2_electoral_swings/provqc22/electoral_swings_distribution_ridges.png", height = 10, width = 12)

