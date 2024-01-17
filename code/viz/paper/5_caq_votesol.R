# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
ResultsQc125 <- left_join(readRDS("_SharedFolder_article_pot-growth/data/warehouse/step2_electoral_swings/qc125_provqc2022.rds"),
                          readRDS("_SharedFolder_article_pot-growth/data/warehouse/step2_electoral_swings/results_provqc2022.rds"),
                          by = c("riding_id", "party")) %>%
  tidyr::drop_na() %>% 
  mutate(delta = proj_vote - prop_vote) %>% 
  group_by(riding_id) %>% 
  mutate(winner2022 = ifelse(prop_vote == max(prop_vote), 1, 0),
         winnernow = ifelse(proj_vote == max(proj_vote), 1, 0))

DataAgg <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2022/aggregated/potgrowth_votesolidity.rds") %>%
  select(riding_id, party, model, estimate = weighted_mean_estimate, stderr = weighted_stderr) %>%
  left_join(., ResultsQc125, by = c("riding_id", "party")) %>% 
  filter(riding_id != "938") %>% # removing Ungava too uncertain
  tidyr::drop_na() %>% 
  mutate(conf.low = estimate - stderr*1.96,
         conf.high = estimate + stderr*1.96)

CaqWon <- DataAgg %>% 
  filter(party == "CAQ" & model == "vote_solidity" &
           winner2022 == 1) %>% 
  ungroup() %>% 
  select(riding_id, votesolCAQ = estimate)

# Graph ------------------------------------------------------------------

colors <- c("CAQ" = "#00cccc", "PLQ" = "#ED1A2D", "PQ" = "#004C9D", "QS" = "#FF5605", "PCQ" = "purple")

Data <- DataAgg %>% 
  filter(riding_id %in% CaqWon$riding_id &
           party != "CAQ" & model == "potgrowth") %>% 
  select(riding_id, riding_name, party, potgrowth = estimate, prop_vote22 = prop_vote,
         delta, winnernow) %>% 
  mutate(prop_vote22 = prop_vote22 * 100,
         potgrowth = potgrowth * 10) %>% 
  tidyr::pivot_longer(., cols = c("potgrowth", "prop_vote22"),
                      names_to = "method") %>% 
  left_join(., CaqWon, by = "riding_id") %>% 
  mutate(winnernow = ifelse(winnernow == 1, "Leading as of\nlatest projections", "Still behind"),
         method = ifelse(method == "potgrowth", "Potential for Growth Estimate", "Vote share in 2022 (%)"))

means <- Data %>% 
  #filter(method == "Potential for Growth Estimate") %>% 
  group_by(party, method) %>% 
  summarise(mean = mean(value))

Data %>% 
  mutate(delta = delta*100) %>% 
  ggplot(aes(x = reorder(riding_name, votesolCAQ), y = value)) +
  facet_wrap(~ method,
             scales = "free_y",
             strip.position = "left") +
  geom_hline(data = means, aes(yintercept = mean,
                               color = party),
             show.legend = F, linetype = "dashed",
             linewidth = 0.5, alpha = 0.4) +
  geom_point(aes(color = party,
                 size = delta),
             alpha = 0.4,
             shape = 19,
             stroke = NA) +
  guides(color = "none",
         size = guide_legend(title = "Gains by the challenger since\n2022 (vote shares %)",
                             title.position = "top")) +
  scale_color_manual(values = colors) +
  scale_size_continuous(range = c(0.05, 6)) +
  scale_x_discrete(expand = c(0.015, 0.015)) +
  ylab("") +
  xlab("\nRidings won by the CAQ\n(ordered from least to most solid)") +
  clessnverse::theme_clean_light() +
  labs(caption = "Dashed lines represent the mean of the measure.\nUngava riding was removed due to the uncertainty of its estimate.") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 15),
        axis.title.y = element_text(hjust = 0.5),
        legend.title = element_text(),
        strip.placement = "outside",
        strip.text.y = element_text(size = 15),
        panel.grid.major.x = element_line(color = "grey85", linewidth = 0.15),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
        axis.text.y = element_text(size = 11))

ggsave("_SharedFolder_article_pot-growth/graphs/paper/5_caq_votesol.png", width = 15.5, height = 12.5)

Rankings <- Data %>% 
  group_by(riding_id, method) %>% 
  mutate(rank = rank(desc(value)))

AggRankings <- Rankings %>% 
  group_by(party, method, rank) %>% 
  summarise(n = n())

DiffFirst <- Data %>% 
  #filter(method == "Potential for Growth Estimate") %>% 
  group_by(riding_id, method) %>% 
  mutate(rank = rank(-value, ties.method = "random")) %>% 
  filter(rank %in% c(1, 2)) %>% 
  tidyr::pivot_wider(., id_cols = c("riding_id", "riding_name", "method"),
                     names_from = "rank",
                     values_from = c("party", "value"))

### different orders
Data %>% 
  #filter(method == "Potential for Growth Estimate") %>% 
  left_join(., DiffFirst, by = c("riding_id", "riding_name", "method")) %>% 
  group_by(method) %>% 
  mutate(delta = delta*100,
         delta_value = value_1 - value_2,
         rank_xaxis = rank(-value_1)) %>% 
  ggplot(aes(x = tidytext::reorder_within(x = riding_name, by = rank_xaxis,
                                          within = method),
             #reorder(riding_name, -value_1),
             y = value)) +
  facet_wrap(~ method,
             scales = "free",
             strip.position = "left") +
  geom_hline(data = means,
             aes(yintercept = mean,
                 color = party),
             show.legend = F, linetype = "dashed",
             linewidth = 0.5, alpha = 0.4) +
  geom_linerange(aes(ymin = value_2 + 0.001, ymax = value_1 - 0.001,
                     color = party_1),
                 alpha = 0.035, linewidth = 2.25) +
  geom_point(aes(color = party,
                 size = delta),
             alpha = 0.4,
             shape = 19,
             stroke = NA) +
  guides(color = "none",
         size = guide_legend(title = "Gains by the challenger since\n2022 (vote shares %)",
                             title.position = "top")) +
  scale_color_manual(values = colors) +
  scale_size_continuous(range = c(0.045, 4.5)) +
  tidytext::scale_x_reordered(expand = c(0.015, 0.015)) +
  #scale_x_discrete() +
  ylab("") +
  xlab("\nRidings won by the CAQ") +
  clessnverse::theme_clean_light() +
  labs(caption = "Dashed lines represent the mean of the measure.\nColored bars represent the gap between the first and second party in the riding.\nUngava riding was removed due to the uncertainty of its estimate.") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 15),
        axis.title.y = element_text(hjust = 0.5),
        legend.title = element_text(),
        strip.placement = "outside",
        strip.text.y = element_text(size = 15),
        panel.grid.major.x = element_line(color = "grey85", linewidth = 0.15),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
        axis.text.y = element_text(size = 11))

ggsave("_SharedFolder_article_pot-growth/graphs/paper/5_caq_votesol2.png", width = 15.5, height = 12.5)

orders <- Data %>% 
  filter(method == "Potential for Growth Estimate") %>%
  group_by(riding_id) %>% 
  filter(value == max(value)) %>% 
  ungroup() %>% 
  mutate(rank_xaxis = rank(-value)) %>% 
  select(riding_id, rank_xaxis)

### same order for facets
Data %>% 
  left_join(., DiffFirst, by = c("riding_id", "riding_name", "method")) %>% 
  mutate(delta = delta*100,
         delta_value = value_1 - value_2) %>% 
  left_join(., orders, by = "riding_id") %>% 
  ggplot(aes(x = reorder(riding_name, rank_xaxis),
             y = value)) +
  facet_wrap(~ method,
             scales = "free_y",
             strip.position = "left") +
  geom_hline(data = means,
             aes(yintercept = mean,
                               color = party),
             show.legend = F, linetype = "dashed",
             linewidth = 0.5, alpha = 0.4) +
  geom_linerange(aes(ymin = value_2 + 0.001, ymax = value_1 - 0.001,
                     color = party_1),
                 alpha = 0.035, linewidth = 2.25) +
  geom_point(aes(color = party,
                 size = delta
                 ),
             alpha = 0.4,
             shape = 19,
             stroke = NA) +
  guides(color = "none",
         size = guide_legend(title = "Gains by the challenger since\n2022 (vote shares %)",
                             title.position = "top")) +
  scale_color_manual(values = colors) +
  scale_size_continuous(range = c(1, 3.5),
                        breaks = c(-10, 0, 10, 20, 30)) +
  scale_x_discrete(expand = c(0.015, 0.015)) +
  ylab("") +
  xlab("\nRidings won by the CAQ") +
  clessnverse::theme_clean_light() +
  labs(caption = "Dashed lines represent the mean of the measure.\nColored bars represent the gap between the first and second party in the riding.\nUngava riding was removed due to the uncertainty of its estimate.") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 15),
        axis.title.y = element_text(hjust = 0.5),
        legend.title = element_text(),
        strip.placement = "outside",
        strip.text.y = element_text(size = 15),
        panel.grid.major.x = element_line(color = "grey85", linewidth = 0.15),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
        axis.text.y = element_text(size = 11))

ggsave("_SharedFolder_article_pot-growth/graphs/paper/5_caq_votesol.png", width = 15.5, height = 12.5)


#Data %>% 
#  mutate(delta = delta*100) %>% 
#  ggplot(aes(x = votesolCAQ, y = value)) +
#  facet_wrap(~ method,
#             scales = "free_y",
#             strip.position = "left") +
#  geom_point(aes(color = party,
#                 size = delta),
#             alpha = 0.4, shape = 19, stroke = NA) +
#  guides(color = "none",
#         size = guide_legend(title = "Gains by the challenger since\n2022 (vote shares %)",
#                             title.position = "top")) +
#  scale_color_manual(values = colors) +
#  scale_size_continuous(range = c(0.05, 5)) +
#  scale_shape_manual(values = c(19, 18)) +
#  ylab("") +
#  xlab("\nCAQ Vote solidity estimate\n") +
#  ggtitle("Potential for growth of challenger parties\nin ridings won by CAQ, the incumbent") +
#  clessnverse::theme_clean_light() +
#  theme(axis.title.x = element_text(hjust = 0.5, size = 15),
#        axis.title.y = element_text(hjust = 0.5),
#        legend.title = element_text(),
#        strip.placement = "outside",
#        strip.text.y = element_text(size = 15))

