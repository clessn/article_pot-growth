# packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# data loading ------------------------------------------------------------
df <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/step2_electoral_swings/qc125_provqc2022.rds")
df_vote <- readxl::read_xlsx("_SharedFolder_article_pot-growth/data/warehouse/step2_electoral_swings/qc125_votes_share_provqc2022.xlsx")
# data wrangling ----------------------------------------------------------

df_seat <- df %>% 
  group_by(riding_id) %>% 
  top_n(1, proj_vote) %>% 
  ungroup %>% 
  count(party) %>% 
  mutate(seat_share = (n / sum(n)) * 100) %>% 
  select(-n) %>% 
  rename(share = seat_share) %>% 
  mutate(facet = "Seats share")

levels <- c("Votes share", "Seats share")

df_graph <- rbind(df_vote, df_seat) 

df_graph$facet <- factor(df_graph$facet, levels = levels)

df_graph$party[df_graph$party == "PLQ"] <- "QLP"
df_graph$party[df_graph$party == "PCQ"] <- "CPQ"

# graphs ------------------------------------------------------------------

ggplot(df_graph, aes(x = reorder(party, +share), y = share, fill = party, color = party)) +
  geom_bar(stat = "identity", alpha = 0.4, color = NA) +
  geom_text(aes(label = paste0(round(share), "%")), hjust = -0.2, position = position_dodge(0.9), size = 8) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_y_continuous(limits = c(0, 52)) +
  labs(title = "", x = "", y = "Share (%)") +
  coord_flip() +
  facet_grid(cols = vars(facet), switch = "x") + 
  clessnverse::theme_clean_light() +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        strip.placement = "outside")



ggsave("_SharedFolder_article_pot-growth/graphs/paper/2_projections_QC125.png", height = 7, width = 12)




