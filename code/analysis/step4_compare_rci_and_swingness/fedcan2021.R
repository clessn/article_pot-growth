# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
Swingness <- readRDS("_SharedFolder_article_pot-growth/data/marts/electoral_swings/fedcan2021.rds")

Rci <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/fedcan2021//aggregated.rds")

Ridings <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  select(riding_id, riding_name)

# Join --------------------------------------------------------------------

Data <- Rci %>% 
  mutate(riding_id = as.character(riding_id)) %>%
  left_join(., Swingness, by = c("riding_id", "party"))


# Graph -------------------------------------------------------------------

fedcan21_colors <- c("PLC" = "#D71B1E", "PCC" = "#142E52", "NPD" = "#F58220", "BQ" = "#080236", "PVC" = "#3D9B35")

ggplot(Data, aes(x = weighted_mean_estimate, y = electoral_swingness)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_smooth(method = "lm") +
  xlab("IRC moyen estimé")

ggsave("_SharedFolder_article_pot-growth/graphs/step4_compare_rci_swingness/explo/fedcan2021/all.png")

ggplot(Data, aes(x = weighted_mean_estimate, y = electoral_swingness)) +
  geom_point(aes(color = party),
             show.legend = FALSE) +
  scale_color_manual(values = fedcan21_colors) +
  geom_smooth(method = "lm") +
  xlab("IRC moyen estimé") +
  facet_wrap(~party)

ggsave("_SharedFolder_article_pot-growth/graphs/step4_compare_rci_swingness/explo/fedcan2021/byparty.png",
       width = 8, height = 6)
