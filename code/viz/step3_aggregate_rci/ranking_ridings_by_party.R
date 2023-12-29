# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------

Fedcan21 <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/fedcan2021/aggregated.rds")
Provqc22 <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2022/aggregated.rds")

Ridings <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  select(level, riding_id, riding_name, prov_terr, large)

# Graph it ----------------------------------------------------------------

## Prov qc 2022 ------------------------------------------------------------

provqc22_colors <- c("CAQ" = "#00cccc","PLQ" = "#FF0024","PQ" = "#099FFF",
                     "QS" = "#FF6600","PCQ"="purple")

for (i in names(provqc22_colors)){
  colori <- provqc22_colors[i]
  plot <- Provqc22 %>%
    filter(party == i) %>% 
    mutate(conf_low = ifelse(conf_low <= -1, -1, conf_low),
           conf_high = ifelse(conf_high >= 1, 1, conf_high),
           weighted_mean_estimate = ifelse(weighted_mean_estimate <= -1, -1, weighted_mean_estimate),
           weighted_mean_estimate = ifelse(weighted_mean_estimate >= 1, 1, weighted_mean_estimate)) %>% 
    left_join(., Ridings, by = "riding_id") %>% 
    ggplot(aes(x = weighted_mean_estimate, y = reorder(riding_name, weighted_mean_estimate))) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_linerange(aes(xmin = conf_low, xmax = conf_high),
                   linewidth = 2.25,
                   alpha = 0.6,
                   color = colori) +
    geom_point(size = 3,
               alpha = 0.6, color = colori) +
    scale_x_continuous(limits = c(-1, 1)) +
    clessnverse::theme_clean_light() +
    ylab("") +
    xlab("IRC moyen estimé") +
    ggtitle(i) +
    theme(axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 18),
          axis.title.x = element_text(size = 24, hjust = 0.5))
  ggsave(plot,
         filename = paste0("_SharedFolder_article_pot-growth/graphs/step3_aggregate_rci/ranking_ridings_by_party/provqc2022/", i, ".png"),
         width = 12, height = 22)
}

## Fed can 2021 ------------------------------------------------------------

fedcan21_colors <- c("PLC" = "#D71B1E", "PCC" = "#142E52", "NPD" = "#F58220", "BQ" = "#080236", "PVC" = "#3D9B35")

Ridingsfed <- Ridings %>% filter(level == "fed2021")

for (i in names(fedcan21_colors)){
  colori <- fedcan21_colors[i]
  if (i == "BQ"){
    data <- Fedcan21 %>% 
      filter(substr(riding_id, 1, 2) == "24")
    plot <- data %>%
      filter(party == i) %>% 
      mutate(conf_low = ifelse(conf_low <= -1, -1, conf_low),
             conf_high = ifelse(conf_high >= 1, 1, conf_high),
             weighted_mean_estimate = ifelse(weighted_mean_estimate <= -1, -1, weighted_mean_estimate),
             weighted_mean_estimate = ifelse(weighted_mean_estimate >= 1, 1, weighted_mean_estimate)) %>% 
      left_join(., Ridingsfed, by = "riding_id") %>% 
      ggplot(aes(x = weighted_mean_estimate, y = reorder(riding_name, weighted_mean_estimate))) +
      geom_vline(xintercept = 0, linetype = "dotted") +
      geom_linerange(aes(xmin = conf_low, xmax = conf_high),
                     linewidth = 2.25,
                     alpha = 0.6,
                     color = colori) +
      geom_point(size = 3,
                 alpha = 0.6, color = colori) +
      scale_x_continuous(limits = c(-1, 1)) +
      clessnverse::theme_clean_light() +
      ylab("") +
      xlab("IRC moyen estimé") +
      ggtitle(i) +
      theme(axis.text.y = element_text(size = 14),
            axis.text.x = element_text(size = 18),
            axis.title.x = element_text(size = 24, hjust = 0.5))
    ggsave(plot,
             filename = paste0("_SharedFolder_article_pot-growth/graphs/step3_aggregate_rci/ranking_ridings_by_party/fedcan2021/", i, ".png"),
             width = 12, height = 22)
  } else {
    data <- Fedcan21
    for (j in unique(Ridingsfed$large)){
      plot <- data %>%
        filter(party == i) %>%
        left_join(., Ridingsfed, by = "riding_id") %>% 
        filter(large == j) %>% 
        mutate(conf_low = ifelse(conf_low <= -1, -1, conf_low),
               conf_high = ifelse(conf_high >= 1, 1, conf_high),
               weighted_mean_estimate = ifelse(weighted_mean_estimate <= -1, -1, weighted_mean_estimate),
               weighted_mean_estimate = ifelse(weighted_mean_estimate >= 1, 1, weighted_mean_estimate)) %>% 
        mutate(riding_label = paste0(prov_terr, " - ", riding_name)) %>% 
        ggplot(aes(x = weighted_mean_estimate, y = reorder(riding_label, weighted_mean_estimate))) +
        geom_vline(xintercept = 0, linetype = "dotted") +
        geom_linerange(aes(xmin = conf_low, xmax = conf_high),
                       linewidth = 2.25,
                       alpha = 0.6,
                       color = colori) +
        geom_point(size = 3,
                   alpha = 0.6, color = colori) +
        scale_x_continuous(limits = c(-1, 1)) +
        clessnverse::theme_clean_light() +
        ylab("") +
        xlab("IRC moyen estimé") +
        ggtitle(paste0(i, " - ", j)) +
        theme(axis.text.y = element_text(size = 14),
              axis.text.x = element_text(size = 18),
              axis.title.x = element_text(size = 24, hjust = 0.5))
      ggsave(plot,
               filename = paste0("_SharedFolder_article_pot-growth/graphs/step3_aggregate_rci/ranking_ridings_by_party/fedcan2021/", i, "-", j,".png"),
               width = 18, height = 25)
  }
  }
}
