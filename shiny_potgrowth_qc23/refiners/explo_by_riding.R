library(dplyr)
library(ggplot2)

data <- readRDS("shiny_potgrowth_qc23/PotGrowthQc2324/data/marts/by_riding.rds")
colors <- c("CAQ" = "#00cccc", "PLQ" = "#ED1A2D", "PQ" = "#004C9D", "QS" = "#FF5605", "PCQ" = "purple")
parties <- names(colors)

regions <- c(
  estrie = "Estrie",
  centre_du_quebec = "Centre-du-\nQuébec",
  monteregie = "Montérégie",
  mtl_west = "Montréal\nOuest",
  mtl_east = "Montréal\nEst",
  laval = "Laval",
  laurentides = "Laurentides",
  lanaudiere = "Lanaudière",
  outaouais = "Outaouais",
  abitibi_temiscamingue = "Abitibi-\nTémiscamingue",
  mauricie = "Mauricie",
  capitale_nationale = "Capitale-\nNationale",
  chaudiere_appalaches = "Chaudière-\nAppalaches",
  bas_saint_laurent = "Bas-Saint-\nLaurent",
  gaspesie_iles_de_la_madeleine = "Gaspésie-Îles-\nde-la-Madeleine",
  cote_nord = "Côte-Nord",
  saguenay_lac_saint_jean = "Saguenay-Lac-\nSaint-Jean",
  "nord-du-quebec" = "Nord-du-\nQuébec"
)

df <- data.frame(t(combn(parties, m = 2)))

for (i in 1:nrow(df)){
  data %>%
    filter(model_proj_vote == "qc125" &
             party %in% c(df$X1[i], df$X2[i])) %>%
    mutate(vote_share = 1 - anti_vote_share) %>%
    ggplot(aes(x = reorder(riding_name, adj_estimate),
               y = adj_estimate)) +
    facet_grid(cols = vars(granular),
               scales = "free_x", space = "free",
               switch = "x") +
    geom_text(aes(label = riding_name, y = adj_estimate - 0.0025,
                  color = party),
              angle = 90, hjust = 1, size = 2,
              position = position_dodge(width = 0.9)) +
    geom_point(aes(alpha = anti_vote_share * 100, color = party, group = party),
               position = position_dodge(width = 0.9),
               stroke = NA, size = 3) +
    scale_fill_manual(values = colors, name = "") +
    scale_color_manual(values = colors, name = "") +
    scale_alpha_continuous(name = "% de votes à aller chercher") +
    xlab("") +
    ylab("Potentiel de croissance\ndes électeurs d'autres partis\n") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(ylim = c(0.45, 0.95)) +
    clessnverse::theme_clean_light() +
    theme(axis.text.x = element_blank(),
          strip.placement = "outside",
          legend.title = element_text())
  ggsave(paste0("shiny_potgrowth_qc23/PotGrowthQc2324/explo/", df$X1[i], "-", df$X2[i], ".png"),
         width = 24, height = 10)
  print(i)
}

data %>%
  filter(model_proj_vote == "qc125") %>%
  mutate(vote_share = 1 - anti_vote_share,
         granular = regions[granular]) %>%
  ggplot(aes(x = reorder(riding_name, adj_estimate),
             y = adj_estimate)) +
  facet_grid(cols = vars(granular),
             scales = "free_x", space = "free",
             switch = "x") +
  geom_text(aes(label = riding_name, y = adj_estimate - 0.0025,
                color = party),
            angle = 90, hjust = 1, size = 2,
            position = position_dodge(width = 0.9)) +
  geom_point(aes(alpha = anti_vote_share * 100, color = party, group = party),
             position = position_dodge(width = 0.9),
             stroke = NA, size = 3) +
  scale_fill_manual(values = colors, name = "") +
  scale_color_manual(values = colors, name = "") +
  scale_alpha_continuous(name = "% de votes à aller chercher") +
  xlab("") +
  ylab("Potentiel de croissance\ndes électeurs d'autres partis\n") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0.45, 0.95)) +
  clessnverse::theme_clean_light() +
  theme(axis.text.x = element_blank(),
        strip.placement = "outside",
        legend.title = element_text(),
        strip.text.x = element_text(size = 6.25))

ggsave(paste0("shiny_potgrowth_qc23/PotGrowthQc2324/explo/all_parties.png"),
       width = 30, height = 10)


# By VI ####
by_profile <- readRDS("shiny_potgrowth_qc23/PotGrowthQc2324/data/marts/by_profile.rds")
## redo using qc125
qc125 <- readRDS("shiny_potgrowth_qc23/PotGrowthQc2324/data/warehouse/qc125_provqc2022.rds") %>% 
  select(-margin)
riding_names_df <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/dimensions/prov_ridings/data.rds") %>% 
  select(riding_id, riding_name)

plot_data <- function(vi){
  by_riding_survey <- by_profile %>% 
    mutate(w_adj_estimate = adj_estimate * prct,
           w_antivoteshare = anti_vote_share * prct,
           w_potgrowth = potgrowth * prct,
           weighted_stderr = std.error^2 * prct^2) %>% 
    group_by_at(c("riding_id", "granular", "party", vi)) %>% 
    summarise(adj_estimate = sum(w_adj_estimate) / sum(prct),
              anti_vote_share = sum(w_antivoteshare) / sum(prct),
              potgrowth = sum(w_potgrowth) / sum(prct),
              weighted_stderr = sqrt(sum(weighted_stderr) / sum(prct)^2 )) %>% 
    mutate(margin_error_95 = 1.96 * weighted_stderr,
           model_proj_vote = "surveydata")
  by_riding_qc125 <- by_riding_survey %>% 
    left_join(., qc125, by = c("riding_id", "party")) %>% 
    mutate(model_proj_vote = "qc125",
           anti_vote_share = 1 - proj_vote,
           ## give 3x more weight to adj_estimate
           potgrowth = adj_estimate^3 * anti_vote_share) %>% 
    select(-proj_vote)
  by_riding <- rbind(by_riding_survey, by_riding_qc125) %>% 
    left_join(., riding_names_df, by = "riding_id")
  return(by_riding)
}

plot <- function(data, vi, category){
  data$vi <- data[[vi]]
  plot <- data %>%
    filter(model_proj_vote == "qc125" &
             vi == category) %>%
    mutate(vote_share = 1 - anti_vote_share,
           granular = regions[granular]) %>%
    ggplot(aes(x = riding_name,
               y = adj_estimate)) +
    facet_grid(cols = vars(granular),
               scales = "free_x", space = "free",
               switch = "x") +
    geom_text(aes(label = riding_name, y = adj_estimate - 0.0025,
                  color = party),
              angle = 90, hjust = 1, size = 2,
              position = position_dodge(width = 0.9)) +
    geom_point(aes(alpha = anti_vote_share * 100, color = party, group = party),
               position = position_dodge(width = 0.9),
               stroke = NA, size = 3) +
    scale_fill_manual(values = colors, name = "") +
    scale_color_manual(values = colors, name = "") +
    scale_alpha_continuous(name = "% de votes à aller chercher") +
    ggtitle(paste0(vi, " - ", category)) +
    xlab("") +
    ylab("Potentiel de croissance\ndes électeurs d'autres partis\n") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(ylim = c(0.4, 1)) +
    clessnverse::theme_clean_light() +
    theme(axis.text.x = element_blank(),
          strip.placement = "outside",
          legend.title = element_text(),
          strip.text.x = element_text(size = 6.25))
  return(plot)
}

plot(plot_data("age"), "age", "34m")
ggsave(paste0("shiny_potgrowth_qc23/PotGrowthQc2324/explo/all_parties_age34m.png"),
       width = 30, height = 10)
plot(plot_data("age"), "age", "3554")
ggsave(paste0("shiny_potgrowth_qc23/PotGrowthQc2324/explo/all_parties_age3554.png"),
       width = 30, height = 10)
plot(plot_data("age"), "age", "55p")
ggsave(paste0("shiny_potgrowth_qc23/PotGrowthQc2324/explo/all_parties_age55p.png"),
       width = 30, height = 10)


plot(plot_data("langue"), "langue", "french")
ggsave(paste0("shiny_potgrowth_qc23/PotGrowthQc2324/explo/all_parties_languefrench.png"),
       width = 30, height = 10)
plot(plot_data("langue"), "langue", "english")
ggsave(paste0("shiny_potgrowth_qc23/PotGrowthQc2324/explo/all_parties_langueenglish.png"),
       width = 30, height = 10)
plot(plot_data("langue"), "langue", "other")
ggsave(paste0("shiny_potgrowth_qc23/PotGrowthQc2324/explo/all_parties_langueother.png"),
       width = 30, height = 10)




