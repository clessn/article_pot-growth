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
Data <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2022/disaggregated.rds") %>% 
  mutate(gender = ifelse(male == 1, "men+", "women+"),
         party = factor(party, levels = c("CAQ", "PLQ", "QS",
                                          "PQ", "PCQ"))) %>% 
  left_join(., Ridings, by = "riding_id")

# aggregated
Agg <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2022/aggregated.rds")



# By profile -------------------------------------------------------------------

## loop for all ridings
for (i in 1:unique(Data$riding_id)){
  riding_idi <- unique(Data$riding_id)[i]
  riding_name <- riding_names[as.character(riding_idi)]
  
  weights <- Data %>% 
    filter(riding_id == riding_idi) %>%
    group_by(party, gender) %>%
    mutate(order = rank(prct),
           group = paste0(age, ".", langue, " ", round(prct*100, digits = 1), "%"),
           gender = ifelse(gender == "men+", "Hommes", "Femmes")) %>% 
    group_by(gender, group) %>% 
    summarise(weight = mean(prct),
              order = mean(order)) %>% 
    mutate(start = -1, end = -1 + weight*6)
  
  p1 <- Data %>%
    filter(riding_id == riding_idi) %>%
    group_by(party, gender) %>%
    mutate(order = rank(prct),
           group = paste0(age, ".", langue, " ", round(prct*100, digits = 1), "%"),
           ## adjust when out of borders
           conf.low = ifelse(conf.low <= -1, -1, conf.low),
           conf.high = ifelse(conf.high >= 1, 1, conf.high),
           estimate = ifelse(estimate <= -1, -1, estimate),
           estimate = ifelse(estimate >= 1, 1, estimate),
           party = factor(party, levels = c("PCQ", "PQ", "QS",
                                            "PLQ", "CAQ")),
           gender = ifelse(gender == "men+", "Hommes", "Femmes")
           ) %>%
    ungroup() %>%
    ggplot(aes(x = estimate,
               y = tidytext::reorder_within(x = group,
                                            by = order,
                                            within = gender),
               color = party)) +
    facet_wrap(~gender,
               scales = "free_y",
               nrow = 1) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_linerange(data = weights,
                   aes(y = tidytext::reorder_within(x = group,
                                                    by = order,
                                                    within = gender),
                       xmin = start, xmax = end),
                   color = "lightgrey", x = 1,
                   linewidth = 6) +
    geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                   linewidth = 2.25, position = position_dodge(width = 0.54),
                   alpha = 0.6) +
    geom_point(size = 3,
               alpha = 0.6,
               position = position_dodge(width = 0.54)) +
    scale_color_manual(values = colors) +
    scale_x_continuous(limits = c(-1, 1)) +
    guides(color = "none") +
    ggtitle(riding_name) +
    tidytext::scale_y_reordered() +
    clessnverse::theme_clean_light() +
    xlab("\nIRC estimé\n") +
    ylab("") +
    theme(axis.title.x = element_text(hjust = 0.5),
          legend.title = element_text(),
          axis.text.y = element_text(size = 12),
          strip.text.x = element_text(size = 12))
  p2 <- Agg %>% 
    mutate(
        ## adjust when out of borders
        conf_low = ifelse(conf_low <= -1, -1, conf_low),
        conf_high = ifelse(conf_high >= 1, 1, conf_high),
        weighted_mean_estimate = ifelse(weighted_mean_estimate <= -1, -1, weighted_mean_estimate),
        weighted_mean_estimate = ifelse(weighted_mean_estimate >= 1, 1, weighted_mean_estimate)) %>% 
    filter(riding_id == riding_idi) %>% 
    ggplot(aes(x = weighted_mean_estimate,
               y = reorder(party, weighted_mean_estimate),
               color = party)) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_segment(aes(yend = reorder(party, weighted_mean_estimate),
                     x = conf_low, xend = conf_high),
                 linewidth = 2.25, alpha = 0.6) +
    geom_point(size = 3) +
    scale_color_manual(values = colors) +
    scale_x_continuous(limits = c(-1, 1)) +
    guides(color = "none") +
    clessnverse::theme_clean_light() +
    xlab("\nIRC moyen estimé\n") +
    ylab("") +
    theme(axis.title.x = element_text(hjust = 0.5),
          legend.title = element_text(),
          axis.text.y = element_text(size = 12))
  plot <- p1 + p2 + plot_layout(ncol = 1, heights = c(2, 1))
  ggsave(plot = plot,
         filename = paste0("_SharedFolder_article_pot-growth/graphs/step3_aggregate_rci/provqc2022/predicted_rci_profile/",
                           riding_idi, ".", riding_name, ".png"),
         width = 9, height = 11)
  message(paste0(i, ": ", riding_name))
}


# By riding ---------------------------------------------------------------

## loop for all ridings
for (i in 1:unique(Data$riding_id)){
  riding_idi <- unique(Data$riding_id)[i]
  riding_name <- riding_names[as.character(riding_idi)]
  p1 <- Data %>%
    filter(riding_id == riding_idi) %>%
    group_by(party) %>%
    mutate(order = rank(estimate),
           group = paste(gender, age, langue, sep = "."),
           ## adjust when out of borders
           conf.low = ifelse(conf.low <= -1, -1, conf.low),
           conf.high = ifelse(conf.high >= 1, 1, conf.high),
           estimate = ifelse(estimate <= -1, -1, estimate),
           estimate = ifelse(estimate >= 1, 1, estimate)
    ) %>%
    ungroup() %>%
    ggplot(aes(x = estimate,
               y = tidytext::reorder_within(x = group,
                                            by = order,
                                            within = party),
               color = party)) +
    lemon::facet_rep_wrap(~party,
                          scales = "free_y",
                          nrow = 2,
                          repeat.tick.labels = "bottom") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_segment(aes(alpha = prct, yend = tidytext::reorder_within(x = group,
                                                                   by = order,
                                                                   within = party),
                     x = conf.low, xend = conf.high),
                 linewidth = 2) +
    geom_point(size = 2) +
    scale_color_manual(values = colors) +
    scale_alpha_continuous(
      name = "Poids dans la circonscription",
      range = c(0.2, 1),
      trans = "log10",
      labels = scales::label_number(scale = 100,
                                    suffix = "%")) +
    scale_x_continuous(limits = c(-1, 1)) +
    guides(color = "none") +
    ggtitle(riding_name) +
    tidytext::scale_y_reordered() +
    clessnverse::theme_clean_light() +
    xlab("\nIRC estimé\n") +
    ylab("") +
    theme(axis.title.x = element_text(hjust = 0.5),
          legend.title = element_text())
  ggsave(plot = p1,
         filename = paste0("_SharedFolder_article_pot-growth/graphs/step3_aggregate_rci/provqc2022/predicted_rci_riding/",
                           riding_idi, ".", riding_name, ".disaggregated.png"),
         width = 11, height = 9)
  p2 <- Agg %>% 
    mutate(
      ## adjust when out of borders
      conf_low = ifelse(conf_low <= -1, -1, conf_low),
      conf_high = ifelse(conf_high >= 1, 1, conf_high),
      weighted_mean_estimate = ifelse(weighted_mean_estimate <= -1, -1, weighted_mean_estimate),
      weighted_mean_estimate = ifelse(weighted_mean_estimate >= 1, 1, weighted_mean_estimate)) %>% 
    filter(riding_id == riding_idi) %>% 
    ggplot(aes(x = weighted_mean_estimate,
               y = reorder(party, weighted_mean_estimate),
               color = party)) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_segment(aes(yend = reorder(party, weighted_mean_estimate),
                     x = conf_low, xend = conf_high),
                 linewidth = 2.25, alpha = 0.6) +
    geom_point(size = 3) +
    scale_color_manual(values = colors) +
    scale_x_continuous(limits = c(-1, 1)) +
    guides(color = "none") +
    ggtitle(riding_name) +
    clessnverse::theme_clean_light() +
    xlab("\nIRC moyen estimé\n") +
    ylab("") +
    theme(axis.title.x = element_text(hjust = 0.5),
          legend.title = element_text())
  ggsave(plot = p2,
         filename = paste0("_SharedFolder_article_pot-growth/graphs/step3_aggregate_rci/provqc2022/predicted_rci_riding/",
                           riding_idi, ".", riding_name, ".aggregated.png"),
         width = 5, height = 4)
  message(paste0(i, ": ", riding_name))
}
