# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------

elxn_results_df <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/step2_electoral_swings/results_provqc2022.rds") %>% 
  select(riding_id, party, prop_vote)

caq_won_ridings <- elxn_results_df %>% 
  group_by(riding_id) %>% 
  filter(prop_vote == max(prop_vote) &
           party == "CAQ") %>% 
  pull(., riding_id)

data <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2022/aggregated/potgrowth_votesolidity.rds") %>%
  select(riding_id, party, model, estimate = weighted_mean_estimate, stderr = weighted_stderr) %>%
  filter(riding_id %in% caq_won_ridings &
           model == "potgrowth" &
           party != "CAQ") %>%
  tidyr::drop_na() %>% 
  mutate(conf.low = estimate - stderr*1.96,
         conf.high = estimate + stderr*1.96) %>% 
  left_join(., elxn_results_df, by = c("riding_id", "party")) %>% 
  group_by(riding_id) %>% 
  mutate(rank_potgrowth = rank(-estimate),
         rank_prop_vote = rank(-prop_vote),
         party = ifelse(party == "PLQ", "QLP", party),
         party = ifelse(party == "PCQ", "CPQ", party),
         party = factor(party, levels = c("QLP", "QS", "PQ", "CPQ"))) %>% 
  tidyr::pivot_longer(., cols = starts_with("rank"),
                      names_prefix = "rank_",
                      names_to = "type",
                      values_to = "rank") %>% 
  mutate(type = case_when(
    type == "potgrowth" ~ "Potential for Growth Estimates",
    type == "prop_vote" ~ "Vote share in 2022 (%)"
  ))

# Graph ------------------------------------------------------------------

colors <- c("QLP" = "#ED1A2D", "PQ" = "#004C9D", "QS" = "#FF5605", "CPQ" = "purple")

labels <- data %>% 
  group_by(party, type, rank) %>% 
  summarise(n = n(), .groups = 'drop') %>%
  tidyr::complete(party, type, rank, fill = list(n = 0)) %>% 
  mutate(adjust = ifelse(n <= 5, 3, -3))

data %>% 
  group_by(party, type, rank) %>% 
  summarise(n = n(), .groups = 'drop') %>%
  tidyr::complete(party, type, rank, fill = list(n = 0)) %>% 
  ggplot(aes(x = rank, y = n)) +
  facet_grid(cols = vars(party),
             rows = vars(type),
             switch = "y") +
  ylab("Number of ridings\n") +
  xlab("Rank among challengers in\nridings won by the CAQ") +
  geom_col(color = NA, alpha = 0.5,
           aes(fill = party),
           show.legend = FALSE) +
  geom_text(data = labels,
            aes(color = party,
                label = n,
                y = n + adjust),
            size = 3, show.legend = FALSE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  clessnize::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))

ggsave("_SharedFolder_article_pot-growth/graphs/paper/6_challengers_in_caq_ridings.png",
       width = 8, height = 4.5)
