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

### Join 2022 electoral results
elxnresults22 <- readRDS("_SharedFolder_article_pot-growth/data/marts/electoral_swings/figure1_electoral_results.rds") %>% 
  rename(share_elxn = share)

df_graph2 <- df_graph %>% 
  left_join(., elxnresults22, by = c("party", "facet"))
df_graph2$party <- factor(df_graph2$party, levels = rev(c("CAQ", "QLP", "QS", "PQ", "CPQ")))

# graphs ------------------------------------------------------------------

colors <- c("CAQ" = "#00cccc", "QLP" = "#ED1A2D", "PQ" = "#004C9D", "QS" = "#FF5605", "CPQ" = "purple")

barwidth <- 0.8

ggplot(df_graph2, aes(x = party, y = share, fill = party, color = party)) +
  facet_grid(cols = vars(facet), switch = "x") + 
  geom_col(alpha = 0.4, color = NA, width = barwidth) +
  geom_linerange(aes(xmin = as.numeric(factor(party)) - barwidth / 2,
                     xmax = as.numeric(factor(party)) + barwidth / 2,
                     y = share_elxn),
                 linetype = "dashed", linewidth = 1) +
  geom_text(aes(label = paste0(round(share), "%")), hjust = -0.19, position = position_dodge(0.9), size = 9) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_y_continuous(limits = c(0, 85)) +
  labs(title = "", x = "", y = "Share (%)",
       caption = "Dashed lines represent the party's share in the 2022 Quebec provincial election") +
  coord_flip() +
  clessnverse::theme_clean_light() +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = 0.5, size = 25),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        strip.text.x = element_text(size = 25),
        strip.placement = "outside",
        plot.caption = element_text(size = 15))


ggsave("pres_spsa2024/graphs/pres_spsa24/6_casestudy_projections.png", height = 7, width = 12)




