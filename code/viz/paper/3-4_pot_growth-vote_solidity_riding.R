# packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggridges)

# data loading ------------------------------------------------------------
df <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2022/aggregated/potgrowth_votesolidity.rds")

# data wrangling ----------------------------------------------------------
df_pg <- df %>% 
  filter(model == "potgrowth", riding_id != "938") %>% 
  mutate(weighted_mean_estimate == ifelse(weighted_mean_estimate > 0, 0, weighted_mean_estimate))

df_pg$party[df_pg$party == "PLQ"] <- "QLP"
df_pg$party[df_pg$party == "PCQ"] <- "CPQ"

df_pg$party <- factor(df_pg$party, levels = c("CPQ", "PQ", "QS", "QLP", "CAQ"))

df_vs <- df %>% 
  filter(model == "vote_solidity", riding_id != "938")

df_vs$party[df_vs$party == "PLQ"] <- "QLP"
df_vs$party[df_vs$party == "PCQ"] <- "CPQ"

df_vs$party <- factor(df_vs$party, levels = c("CPQ", "PQ", "QS", "QLP", "CAQ"))

# graphs ------------------------------------------------------------------
colors <- c("CAQ" = "#00cccc", "QLP" = "#ED1A2D", "PQ" = "#004C9D", "QS" = "#FF5605", "CPQ" = "purple")

ggplot(df_pg, aes(x = weighted_mean_estimate * 10, y = party, fill = party, color = party)) +
  geom_density_ridges(alpha = 0.4, color = "white", quantile_lines = TRUE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(title = "", x = "\nEstimated potential for growth using RCI\n", y = "",
       caption = "The quantile lines represent the 25th, 50th and 75th percentiles of the distribution") +
  clessnverse::theme_clean_light() +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.caption = element_text(size = 12)) 

ggsave("_SharedFolder_article_pot-growth/graphs/paper/3_pot_growth_distribution.png", height = 7, width = 10)


ggplot(df_vs, aes(x = weighted_mean_estimate, y = party, fill = party, color = party)) +
  geom_density_ridges(alpha = 0.4, color = "white", quantile_lines = TRUE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(title = "", x = "\nEstimated vote solidity using RCI\n", y = "",
       caption = "The quantile lines represent the 25th, 50th and 75th percentiles of the distribution") +
  clessnverse::theme_clean_light() +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.caption = element_text(size = 12)) 

ggsave("_SharedFolder_article_pot-growth/graphs/paper/4_vote_solidity_distribution.png", height = 7, width = 10)



### memoire
ggplot(df_pg, aes(x = weighted_mean_estimate * 10, y = party, fill = party, color = party)) +
  geom_density_ridges(alpha = 0.4, color = "white", quantile_lines = TRUE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  labs(title = "", x = "\nPotentiel de croissance estimé avec l'IRC\n", y = "",
       caption = "Les lignes de quantile représentent les 25e, 50e et 75e centiles de la distribution.") +
  clessnverse::theme_clean_light() +
  scale_y_discrete(labels = c("CAQ" = "CAQ",
                              "QLP" = "PLQ",
                              "PQ" = "PQ",
                              "QS" = "QS",
                              "CPQ" = "PCQ")) +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.caption = element_text(size = 12)) 

ggsave("_SharedFolder_article_pot-growth/graphs/paper/3_pot_growth_distribution_fr.png", height = 7, width = 10)
