# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
Swingness <- readRDS("_SharedFolder_article_pot-growth/data/marts/electoral_swings/provqc2022.rds")

Rci <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2022/aggregated.rds")

Ridings <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  select(riding_id, riding_name)

# Join --------------------------------------------------------------------

Data <- left_join(Rci, Swingness, by = c("riding_id", "party"))


# Graph -------------------------------------------------------------------

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
