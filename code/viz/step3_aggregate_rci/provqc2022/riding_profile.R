# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(patchwork)

# Data --------------------------------------------------------------------
Ridings <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  select(level, riding_id, riding_name)

riding_names <- Ridings$riding_name[Ridings$level == "prov2022"]
names(riding_names) <- Ridings$riding_id[Ridings$level == "prov2022"]

colors <- c("CAQ" = "#00cccc","PLQ" = "#FF0024","PQ" = "#099FFF",
            "QS" = "#FF6600","PCQ"="purple")

# disaggregated
Data <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2022/disaggregated/potgrowth_votesolidity.rds") %>% 
  mutate(gender = ifelse(male == 1, "men+", "women+"),
         party = factor(party, levels = c("CAQ", "PLQ", "QS",
                                          "PQ", "PCQ"))) %>% 
  left_join(., Ridings, by = "riding_id")

# aggregated
Agg <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2022/aggregated/potgrowth_votesolidity.rds")

## vote int
Voteint <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2022/disaggregated/voteint.rds") %>% 
  mutate(party = factor(party, levels = c("CAQ", "PLQ", "QS",
                                          "PQ", "PCQ")))

# By profile --------------------------------------------------------------

## loop for all ridings
for (i in 1:unique(Data$riding_id)){
  riding_idi <- unique(Data$riding_id)[i]
  riding_name <- riding_names[as.character(riding_idi)]
  
  xlimit <- 1.685
  
  weights <- Data %>% 
    filter(riding_id == riding_idi) %>%
    group_by(party, gender) %>%
    mutate(order = rank(prct),
           group = paste0(age, ".", langue, " ", round(prct*100, digits = 1), "%"),
           gender = ifelse(gender == "men+", "Hommes", "Femmes")) %>% 
    group_by(gender, group) %>% 
    summarise(weight = mean(prct),
              order = mean(order)) %>% 
    mutate(start = -xlimit, end = -xlimit + weight*2.5)
  
  voteinti <- Data %>% 
    filter(riding_id == riding_idi) %>%
    group_by(party, gender) %>%
    mutate(order = rank(prct),
           group = paste0(age, ".", langue, " ", round(prct*100, digits = 1), "%"),
           party = factor(party, levels = c("PCQ", "PQ", "QS",
                                            "PLQ", "CAQ")),
           gender = ifelse(gender == "men+", "Hommes", "Femmes")) %>% 
    left_join(., Voteint, by = c("party", "male", "age", "langue", "riding_id")) %>% 
    mutate(start = xlimit - predicted_vote_share/2.15,
           end = xlimit)
  
  AggVoteInt <- voteinti %>% 
    group_by(party) %>% 
    summarise(vote_share = weighted.mean(x = predicted_vote_share,
                                         w = prct))
  
  axisxlabels <- data.frame(
    x = c(-xlimit + 0.25, -0.1, 0.1, xlimit - 0.25),
    label = c("Poids", "Potentiel\nde croissance",
              "Solidité\ndu vote", "Distribution\nestimée des\nintentions de vote"),
    hjust = c(0, 1, 0, 1)
  )
  
  p1 <- Data %>%
    filter(riding_id == riding_idi) %>%
    group_by(party, gender) %>%
    mutate(order = rank(prct),
           group = paste0(age, ".", langue, " ", round(prct*100, digits = 1), "%"),
           ## adjust when out of borders
           conf.low = ifelse(conf.low <= -1 & model == "potgrowth", -1, conf.low),
           conf.low = ifelse(conf.low <= 0 & model == "vote_solidity", 0, conf.low),
           conf.high = ifelse(conf.high >= 0 & model == "potgrowth", 0, conf.high),
           conf.high = ifelse(conf.high >= 1 & model == "vote_solidity", 1, conf.high),
           estimate = ifelse(estimate <= -1 & model == "potgrowth", -1, estimate),
           estimate = ifelse(estimate >= 0 & model == "potgrowth", 0, estimate),
           estimate = ifelse(estimate >= 1 & model == "vote_solidity", 1, estimate),
           estimate = ifelse(estimate <= 0 & model == "vote_solidity", 0, estimate),
           party = factor(party, levels = c("PCQ", "PQ", "QS",
                                            "PLQ", "CAQ")),
           gender = ifelse(gender == "men+", "Hommes", "Femmes")
    ) %>%
    ungroup() %>%
    ggplot(aes(x = estimate,
               y = tidytext::reorder_within(x = group,
                                            by = order,
                                            within = gender),
               color = party,
               group = interaction(group, party))) +
    facet_wrap(~gender,
               scales = "free_y",
               nrow = 1) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    ## vote int
    geom_linerange(data = voteinti,
                   aes(y = tidytext::reorder_within(x = group,
                                                    by = order,
                                                    within = gender),
                       xmin = start, xmax = end,
                       color = party),
                   alpha = 0.8,
                   linewidth = 2, position = position_dodge(width = 0.54)) +
    geom_text(data = voteinti,
              aes(y = tidytext::reorder_within(x = group,
                                               by = order,
                                               within = gender),
                  x = start-0.025, label = paste0(round(predicted_vote_share*100), "%")),
              color = "black", hjust = 1, size = 2.7,
              position = position_dodge(width = 0.54)) +
    ## weight
    geom_linerange(data = weights,
                   aes(y = tidytext::reorder_within(x = group,
                                                    by = order,
                                                    within = gender),
                       xmin = start, xmax = end, group = group),
                   color = "lightgrey", x = 1,
                   linewidth = 6) +
    geom_linerange(aes(xmin = conf.low, xmax = conf.high,
                       alpha = model),
                   linewidth = 2.25, position = position_dodge(width = 0.54)) +
    geom_point(size = 3,
               alpha = 1,
               position = position_dodge(width = 0.54)) +
    scale_color_manual(values = colors) +
    scale_alpha_manual(values = c("potgrowth" = 0.3,
                                  "vote_solidity" = 0.7)) +
    scale_x_continuous(limits = c(-xlimit, xlimit),
                       expand = c(0, 0)) +
    guides(color = "none", alpha = "none") +
    ggtitle(riding_name) +
    tidytext::scale_y_reordered(expand = c(0.1, 0.005)) +
    clessnverse::theme_clean_light() +
    xlab("") +
    ylab("") +
    geom_text(data = axisxlabels,
              aes(x = x,
                  label = label,
                  hjust = hjust),
              y = 9.75,
              group = 1, color = "black",
              vjust = 1, size = 3.25,
              lineheight = 0.7) +
    geom_text(data = axisxlabels,
              aes(x = x,
                  label = label,
                  hjust = hjust),
              y = 0.25,
              group = 1, color = "black",
              vjust = 0, size = 3.25,
              lineheight = 0.7) +
    theme(axis.title.x = element_text(hjust = 0.5),
          axis.text.x = element_blank(),
          legend.title = element_text(),
          axis.text.y = element_text(size = 12),
          strip.text.x = element_text(size = 12),
          panel.background = element_rect(color = "lightgrey", fill = NA))
  p2 <- Agg %>% 
    mutate(
      ## adjust when out of borders
      conf_low = ifelse(conf_low <= -1 & model == "potgrowth", -1, conf_low),
      conf_low = ifelse(conf_low <= 0 & model == "vote_solidity", 0, conf_low),
      conf_high = ifelse(conf_high >= 0 & model == "potgrowth", 0, conf_high),
      conf_high = ifelse(conf_high >= 1 & model == "vote_solidity", 1, conf_high),
      weighted_mean_estimate = ifelse(weighted_mean_estimate <= -1 & model == "potgrowth", -1, weighted_mean_estimate),
      weighted_mean_estimate = ifelse(weighted_mean_estimate >= 0 & model == "potgrowth", 0, weighted_mean_estimate),
      weighted_mean_estimate = ifelse(weighted_mean_estimate >= 1 & model == "vote_solidity", 1, weighted_mean_estimate),
      weighted_mean_estimate = ifelse(weighted_mean_estimate <= 0 & model == "vote_solidity", 0, weighted_mean_estimate)) %>% 
    filter(riding_id == riding_idi) %>% 
    left_join(., AggVoteInt, by = "party") %>% 
    ggplot(aes(x = weighted_mean_estimate,
               y = reorder(party, vote_share),
               color = party)) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_segment(aes(yend = reorder(party, weighted_mean_estimate),
                     x = conf_low, xend = conf_high,
                     alpha = model),
                 linewidth = 3, alpha = 0.6) +
    geom_linerange(aes(xmin = xlimit - vote_share / 2,
                       xmax = xlimit),
                   linewidth = 4.5) +
    geom_text(aes(x = xlimit - vote_share / 2 - 0.025,
                  label = paste0(round(vote_share*100), "%")),
              color = "black", hjust = 1, size = 3.5) +
    geom_text(data = axisxlabels,
              aes(x = x,
                  label = label,
                  hjust = hjust),
              y = 0.25,
              group = 1, color = "black",
              vjust = 1, size = 3.25,
              lineheight = 0.7) +
    geom_point(size = 5) +
    scale_color_manual(values = colors) +
    scale_alpha_manual(values = c("potgrowth" = 0.3,
                                  "vote_solidity" = 0.7)) +
    scale_x_continuous(limits = c(-1, xlimit)) +
    scale_y_discrete(expand = c(0.4, 0.2)) +
    guides(color = "none") +
    clessnverse::theme_clean_light() +
    xlab("") +
    ylab("") +
    theme(axis.title.x = element_text(hjust = 0.5),
          axis.text.x = element_blank(),
          legend.title = element_text(),
          axis.text.y = element_text(size = 12))
  # Combinaison des plots avec layout personnalisé
  plot <- p1 / (plot_spacer() + p2 + plot_spacer() +
                  plot_layout(widths = c(0.1, 0.8, 0.1))) +
    plot_layout(heights = c(2, 0.55))
  
  ggsave(plot = plot,
         filename = paste0("_SharedFolder_article_pot-growth/graphs/step3_aggregate_rci/provqc2022/riding_profile/",
                           riding_idi, ".", riding_name, ".png"),
         width = 11, height = 13)
  message(paste0(i, ": ", riding_name))
}
