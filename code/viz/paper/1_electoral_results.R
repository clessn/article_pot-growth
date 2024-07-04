# packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# data loading ------------------------------------------------------------
df <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/step2_electoral_swings/results_provqc2022.rds")


# data wrangling ----------------------------------------------------------

df_vote <- df %>% 
  group_by(party) %>% 
  summarise(nvotes = sum(nvotes_party)) %>% 
  mutate(vote_share = (nvotes / sum(nvotes)) * 100) %>% 
  select(-nvotes) %>% 
  rename(share = vote_share) %>% 
  mutate(facet = "Votes share")

df_seat <- df %>% 
  group_by(riding_name) %>% 
  top_n(1, prop_vote) %>% 
  ungroup %>% 
  count(party) %>% 
  mutate(seat_share = (n / sum(n)) * 100)

PCQ <- data.frame(party = "PCQ", n = 0, seat_share = 0)

df_seat <- rbind(df_seat, PCQ) %>% 
  select(-n) %>% 
  rename(share = seat_share) %>% 
  mutate(facet = "Seats share")

levels <- c("Votes share", "Seats share")

df_graph <- rbind(df_vote, df_seat) 

df_graph$facet <- factor(df_graph$facet, levels = levels)

df_graph$party[df_graph$party == "PLQ"] <- "QLP"
df_graph$party[df_graph$party == "PCQ"] <- "CPQ"

saveRDS(df_graph, "_SharedFolder_article_pot-growth/data/marts/electoral_swings/figure1_electoral_results.rds")

# graphs ------------------------------------------------------------------

colors <- c("CAQ" = "#00cccc", "QLP" = "#ED1A2D", "PQ" = "#004C9D", "QS" = "#FF5605", "CPQ" = "purple")

ggplot(df_graph, aes(x = reorder(party, share), y = share, fill = party, color = party)) +
  geom_col(width = 0.8, alpha = 0.4, color = NA) +
  geom_text(aes(label = paste0(round(share), "%")), hjust = -0.2, position = position_dodge(0.9), size = 8) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_y_continuous(limits = c(0, 80)) +
  labs(title = "", x = "", y = "Share (%)",
       caption = " ") +
  coord_flip() +
  facet_grid(cols = vars(facet), switch = "x") + 
  clessnize::theme_clean_light() +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        strip.placement = "outside",
        plot.caption = element_text(size = 12))



ggsave("_SharedFolder_article_pot-growth/graphs/paper/1_electoral_results.png", height = 7, width = 12)
  


  
