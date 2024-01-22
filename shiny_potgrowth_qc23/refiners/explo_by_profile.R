# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
data <- readRDS("shiny_potgrowth_qc23/PotGrowthQc2324/data/marts/by_profile.rds") %>% 
  left_join(., readRDS("_SharedFolder_article_pot-growth/data/warehouse/dimensions/prov_ridings/data.rds") %>% 
              select(-riding_name),
            by = "riding_id") %>% 
  mutate(n = prct * pop_n) %>% 
  group_by(granular) %>% 
  mutate(prct_region = n / sum(n)) %>% 
  group_by(granular, party, age, langue) %>%
  summarise(adj_estimate = sum(prct_region * adj_estimate) / sum(prct_region),
            anti_vote_share = sum(prct_region * anti_vote_share) / sum(prct_region),
            prct = sum(prct_region) * 5)

weights_profiles_riding <- readRDS("shiny_potgrowth_qc23/PotGrowthQc2324/data/marts/by_profile.rds")

colors <- c("CAQ" = "#00cccc", "PLQ" = "#ED1A2D", "PQ" = "#004C9D", "QS" = "#FF5605", "PCQ" = "purple", "other" = "grey60")
parties <- names(colors)[names(colors) != "other"]

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

weights <- data %>% 
  mutate(granular = regions[granular]) %>% 
  group_by(age, langue, granular) %>% 
  summarise(weight = mean(prct))
  
## all

### make function for plot
data %>% 
  mutate(granular = regions[granular],
         age = factor(age, levels = c("34m", "3554", "55p")),
         langue = factor(langue, levels = c("french", "english", "other"))) %>%
  ggplot(aes(x = age, y = adj_estimate)) +
  geom_col(data = weights, aes(y = weight),
           fill = "grey80") +
  geom_jitter(aes(color = party, size = anti_vote_share),
              stroke = NA, height = 0.2, width = 0,
              alpha = 0.6) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_size_continuous(range = c(1.5, 8)) +
  lemon::facet_rep_grid(cols = vars(granular),
                        rows = vars(langue),
                        repeat.tick.labels = "x",
                        switch = "y") +
  xlab("") +
  clessnverse::theme_clean_light() +
  theme(strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14),
        axis.text.x =  element_text(size = 14))

ggsave("shiny_potgrowth_qc23/PotGrowthQc2324/explo/by_region/all.png",
       width = 35, height = 15)

## by party

for (i in parties){
  data %>% 
    mutate(granular = regions[granular],
           age = factor(age, levels = c("34m", "3554", "55p")),
           langue = factor(langue, levels = c("french", "english", "other")),
           party = ifelse(party == i, i, "other")) %>%
    ggplot(aes(x = age, y = adj_estimate)) +
    geom_col(data = weights, aes(y = weight),
             fill = "grey80") +
    #geom_point(aes(color = party, size = anti_vote_share),
    #           stroke = NA,
    #           position = position_dodge(width = 0.7)) +
    geom_jitter(aes(color = party, size = anti_vote_share),
                stroke = NA, height = 0.2, width = 0,
                alpha = 0.6) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    #scale_alpha_continuous(range = c(0.25, 1)) +
    scale_size_continuous(range = c(1.5, 8)) +
    lemon::facet_rep_grid(cols = vars(granular),
                          rows = vars(langue),
                          repeat.tick.labels = "x",
                          switch = "y") +
    xlab("") +
    clessnverse::theme_clean_light() +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14),
          #panel.grid.major.x = element_line(linewidth = 3, color = "grey90"),
          axis.text.x =  element_text(size = 14))
  ggsave(paste0("shiny_potgrowth_qc23/PotGrowthQc2324/explo/by_region/all", i, ".png"),
         width = 35, height = 15)
  message(i)
}


hist(data$adj_estimate[data$party == "QS"])

data$adj_estimate_qt <- ntile(data$adj_estimate, 100)/100
hist(data$adj_estimate_qt)


for (i in parties){
  data %>%
    mutate(region = regions[granular],
           region = factor(region,
                           levels = c(
                             "Abitibi-\nTémiscamingue",
                             "Nord-du-\nQuébec",                
                             "Saguenay-Lac-\nSaint-Jean",
                             "Côte-Nord",
                             "Bas-Saint-\nLaurent",
                             "Gaspésie-Îles-\nde-la-Madeleine",
                             "Outaouais",                    
                             "Laurentides",
                             "Lanaudière",
                             "Mauricie",                     
                             "Capitale-\nNationale",
                             "Chaudière-\nAppalaches",
                             "Montréal\nOuest",
                             "Laval",
                             "Montréal\nEst",
                             "Montérégie",                   
                             "Estrie",
                             "Centre-du-\nQuébec"
                           )),
           vote_share = 1 - anti_vote_share,
           vote_share_prct = vote_share * prct,
           age = factor(age, levels = c("34m", "3554", "55p")),
           langue = factor(langue, levels = c("french", "english", "other"))) %>% 
    filter(party == i) %>%
    ungroup() %>% 
    ggplot(aes(x = region, y = prct)) +
    geom_col(width = 0.6, aes(fill = adj_estimate_qt)) +
    geom_col(width = 0.8, aes(y = vote_share_prct), fill = colors[i]) +
    scale_alpha_continuous(range = c(0, 0.9)) +
    scale_fill_gradientn(name = "Potentiel de croissance",
                         colors = c("black",
                                    "#DAA520",
                                    "#50C878",
                                    "#003366"),
                         values = c(0, 0.25, 0.75, 1),
                         labels = c("Faible", "Élevé"),
                         breaks = c(0.2, 0.8),
                         limits = c(0, 1)) +
    scale_y_continuous(labels = c((0:5/10) * 100),
                       breaks = 0:5/10) +
    lemon::facet_rep_grid(rows = vars(age),
                          cols = vars(langue),
                          repeat.tick.labels = "x",
                          switch = "y") +
    geom_text(aes(y = prct + 0.02,
                  label = round(adj_estimate_qt * 100)),
              size = 3.5) +
    xlab("") +
    ylab("\nPoids dans la région (%)\n") +
    ggtitle(i) +
    labs(caption = "Les barres des couleurs du parti représentent les votes acquis dans ce profil.\nLes chiffres indiquent le potentiel de croissance des électeurs restants (de 0 à 100).") +
    clessnverse::theme_clean_light() +
    theme(axis.text.x = element_text(hjust = 1, vjust = 0.5,
                                     lineheight = 0.7, angle = 90),
          legend.title = element_text(),
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14),
          axis.title.y = element_text(size = 17, hjust = 0.5))
  ggsave(paste0("shiny_potgrowth_qc23/PotGrowthQc2324/explo/by_region/byparty_", i, ".png"),
         width = 15, height = 14)
  message(i)
}

# By region ---------------------------------------------------------------

for (i in 1:length(regions)){
  regioni <- regions[i]
  regioni_withoutbreaks <- gsub("\\n", "", regioni)
  data %>%
    mutate(region = regions[granular],
           vote_share = 1 - anti_vote_share,
           vote_share_prct = vote_share * prct,
           age = factor(age, levels = c("34m", "3554", "55p")),
           langue = factor(langue, levels = c("french", "english", "other"))) %>% 
    filter(region == regioni) %>%
    ungroup() %>% 
    ggplot(aes(x = party, y = prct)) +
    geom_col(width = 0.6, aes(fill = adj_estimate_qt)) +
    scale_fill_gradientn(name = "Potentiel de croissance",
                         colors = c("black",
                                    "#DAA520",
                                    "#50C878",
                                    "#003366"),
                         values = c(0, 0.25, 0.75, 1),
                         labels = c("Faible", "Élevé"),
                         breaks = c(0.2, 0.8),
                         limits = c(0, 1)) +
    ggnewscale::new_scale_fill() +
    geom_col(width = 0.8, aes(y = vote_share_prct, fill = party),
             show.legend = FALSE) +
    scale_fill_manual(values = colors) +
    scale_alpha_continuous(range = c(0, 0.9)) +
    scale_y_continuous(labels = c((0:5/10) * 100),
                       breaks = 0:5/10,
                       limits = c(0, 0.5)) +
    lemon::facet_rep_grid(rows = vars(age),
                          cols = vars(langue),
                          repeat.tick.labels = "x",
                          switch = "y") +
    geom_text(aes(y = prct + 0.02,
                  label = round(adj_estimate_qt * 100)),
              size = 3.5) +
    xlab("") +
    ylab("\nPoids dans la région (%)\n") +
    ggtitle(regioni_withoutbreaks) +
    labs(caption = "Les barres des couleurs du parti représentent les votes acquis dans ce profil.\nLes chiffres indiquent le potentiel de croissance des électeurs restants (de 0 à 100).") +
    clessnverse::theme_clean_light() +
    theme(axis.text.x = element_text(hjust = 1, vjust = 0.5,
                                     lineheight = 0.7, angle = 90),
          legend.title = element_text(),
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14),
          axis.title.y = element_text(size = 17, hjust = 0.5))
  ggsave(paste0("shiny_potgrowth_qc23/PotGrowthQc2324/explo/by_region2/", i, "-", regioni_withoutbreaks, ".png"),
         width = 15, height = 14)
  message(regioni_withoutbreaks)
}

