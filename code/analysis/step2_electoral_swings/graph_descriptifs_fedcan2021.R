library(ggplot2)
library(dplyr)
library(ggridges)



# Data loading ------------------------------------------------------------

fedcan2021 <- readRDS("_SharedFolder_article_pot-growth/data/marts/electoral_swings/fedcan2021.rds")


# Data wrangling ----------------------------------------------------------
graphPLC <- fedcan2021 %>% 
  filter(party == "PLC")

graphPCC <- fedcan2021 %>% 
  filter(party == "PCC")

graphNPD <- fedcan2021 %>% 
  filter(party == "NPD")

graphBQ <- fedcan2021 %>% 
  filter(party == "BQ")

graphPVC <- fedcan2021 %>% 
  filter(party == "PVC")

# graphs ------------------------------------------------------------------

ggplot(fedcan2021, aes(x = electoral_swingness)) +
  geom_density(size = 1, color = "#452da7", fill = "#452da7", alpha = 0.8) +
  clessnverse::theme_clean_light() +
  labs(title = "Electoral swingness distribution", 
       x = "electoral swingness", 
       y = "") +
  geom_vline(xintercept = 0) +
  xlim(-0.45, 0.65)
  
# PLC

ggplot(graphPLC, aes(x = electoral_swingness)) +
  geom_density(size = 1, color = "#D71B1E", fill = "#D71B1E", alpha = 0.8) +
  clessnverse::theme_clean_light() +
  labs(title = "Electoral swingness distribution, Liberal Party of Canada", 
       x = "electoral swingness", 
       y = "") +
  geom_vline(xintercept = 0) +
  xlim(-0.45, 0.65)

# PCC

ggplot(graphPCC, aes(x = electoral_swingness)) +
  geom_density(size = 1, color = "#142E52", fill = "#142E52", alpha = 0.8) +
  clessnverse::theme_clean_light() +
  labs(title = "Electoral swingness distribution, Conservative Party of Canada", 
       x = "electoral swingness", 
       y = "") +
  geom_vline(xintercept = 0) +
  xlim(-0.45, 0.65)

# NPD

ggplot(graphNPD, aes(x = electoral_swingness)) +
  geom_density(size = 1, color = "#F58220", fill = "#F58220", alpha = 0.8) +
  clessnverse::theme_clean_light() +
  labs(title = "Electoral swingness distribution, New Democratic Party", 
       x = "electoral swingness", 
       y = "") +
  geom_vline(xintercept = 0) +
  xlim(-0.45, 0.65)

# BQ

ggplot(graphBQ, aes(x = electoral_swingness)) +
  geom_density(size = 1, color = "#080236", fill = "#080236", alpha = 0.8) +
  clessnverse::theme_clean_light() +
  labs(title = "Electoral swingness distribution, Bloc Québécois", 
       x = "electoral swingness", 
       y = "") +
  geom_vline(xintercept = 0) +
  xlim(-0.45, 0.65)

# PVC

ggplot(graphPVC, aes(x = electoral_swingness)) +
  geom_density(size = 1, color = "#3D9B35", fill = "#3D9B35", alpha = 0.8) +
  clessnverse::theme_clean_light() +
  labs(title = "Electoral swingness distribution, Green Party of Canada", 
       x = "electoral swingness", 
       y = "") +
  geom_vline(xintercept = 0) +
  xlim(-0.45, 0.65)

# combined
colors <- c("PLC" = "#D71B1E", "PCC" = "#142E52", "NPD" = "#F58220", "BQ" = "#080236", "PVC" = "#3D9B35")

ggplot(fedcan2021, aes(x = electoral_swingness, color = NA, fill = party, alpha = 0.8)) +
  geom_density() +
  facet_wrap(~party) +
  scale_color_manual(values = NA) +
  scale_fill_manual(values = colors) +
  clessnverse::theme_clean_light() +
  labs(title = "Electoral swingness distribution, by party", 
       x = "electoral swingness", 
       y = "") +
  geom_vline(xintercept = 0) +
  xlim(-0.45, 0.65) +
  theme(legend.position = "none")


# Save --------------------------------------------------------------------

ggsave("_SharedFolder_article_pot-growth/graphs/step2_electoral_swings/fedcan2021/electoral_swings_distribution.png", height = 10, width = 12)

# Graph ggridges

ggplot(fedcan2021, aes(x = electoral_swingness, y = party, color = NA, fill = party, alpha = 0.8)) +
  geom_density_ridges()+
  scale_color_manual(values = NA) +
  scale_fill_manual(values = colors) +
  clessnverse::theme_clean_light() +
  labs(title = "Electoral swingness distribution, by party", 
       x = "electoral swingness", 
       y = "") +
  geom_vline(xintercept = 0) +
  xlim(-0.45, 0.65) +
  theme(legend.position = "none")


# Save --------------------------------------------------------------------

ggsave("_SharedFolder_article_pot-growth/graphs/step2_electoral_swings/fedcan2021/electoral_swings_distribution_ridges.png", height = 10, width = 12)
