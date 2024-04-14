# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
ridings_df <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>%
  filter(level == "prov2022") %>% 
  select(riding_id, granular)

data <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/rcis_prov.rds") %>% 
  left_join(., ridings_df, by = "riding_id") %>% 
  filter(rci_CAQ >= 0) %>% 
  ## we want to test the rci as a numeric variable and as an unordered factor 
  mutate(rci_CAQ_factor = factor(rci_CAQ, ordered = FALSE),
         rci_CAQ_factor_ordered = factor(rci_CAQ, ordered = TRUE)) %>% 
  rename(rci_QLP = rci_PLQ,
         rci_CPQ = rci_PCQ) %>% 
  select(-female) %>% 
  tidyr::drop_na()


# Models ------------------------------------------------------------------

for (i in c("PQ", "QLP", "QS", "CPQ")){
  mdata <- data
  mdata$vd <- data[[paste0("rci_", i)]]
  model_num <- lm(vd ~ rci_CAQ + granular + age + langue + male + educ,
                  data = mdata)
  model_factor <- lm(vd ~ rci_CAQ_factor + granular + age + langue + male + educ,
                     data = mdata)
  preds_num <- marginaleffects::predictions(model = model_num,
                                            newdata = marginaleffects::datagrid(rci_CAQ = (0:10)/10),
                                            conf_level = 0.95) %>%
    mutate(conf_level = "95") %>%
    rbind(marginaleffects::predictions(model = model_num,
                                       newdata = marginaleffects::datagrid(rci_CAQ = (0:10)/10),
                                       conf_level = 0.99) %>% 
            mutate(conf_level = "99")) %>%
    mutate(rci_class = "numeric") %>%
    select(rci_class, rci_CAQ, estimate, conf.low, conf.high, conf_level)
  preds_factor <- marginaleffects::predictions(model = model_factor,
                                               newdata = marginaleffects::datagrid(rci_CAQ_factor = unique(mdata$rci_CAQ_factor)),
                                               conf_level = 0.95) %>%
    mutate(conf_level = "95") %>%
    rbind(marginaleffects::predictions(model = model_factor,
                                       newdata = marginaleffects::datagrid(rci_CAQ_factor = unique(mdata$rci_CAQ_factor)),
                                       conf_level = 0.99) %>% 
            mutate(conf_level = "99")) %>%
    mutate(rci_class = "factor",
           rci_CAQ = as.numeric(as.character(rci_CAQ_factor))) %>% 
    select(rci_class, rci_CAQ, estimate, conf.low, conf.high, conf_level)
  predsi <- rbind(preds_num, preds_factor) %>% 
    mutate(party = i) %>% 
    tidyr::pivot_wider(names_from = conf_level, values_from = c(conf.low, conf.high))
  if (i == "PQ"){
    preds_df <- predsi
  } else {
    preds_df <- rbind(preds_df, predsi)
  }
  message(i)
}


# Graph -------------------------------------------------------------------

colors <- c("QLP" = "#ED1A2D", "PQ" = "#004C9D", "QS" = "#FF5605", "CPQ" = "purple")

preds_df %>% 
  filter(rci_class != "factor_ordered") %>% 
  mutate(party = factor(party, levels = c("QLP", "QS", "PQ", "CPQ")),
         rci_class = case_when(
           rci_class == "factor" ~ "RCI for the CAQ:\nCategorical",
           rci_class == "numeric" ~ "RCI for the CAQ:\nNumerical"
         )) %>% 
  ggplot(aes(x = rci_CAQ, y = estimate)) +
  facet_wrap(~rci_class) +
  geom_line(aes(group = party, color = party),
            show.legend = FALSE,
            alpha = 0.3) +
  geom_linerange(aes(group = party, color = party,
                     ymin = conf.low_95, ymax = conf.high_95),
                 alpha = 1, linewidth = 0.95) +
  geom_linerange(aes(group = party, color = party,
                     ymin = conf.low_99, ymax = conf.high_99),
                 alpha = 0.5, linewidth = 0.55) +
  geom_point(aes(color = party),
             size = 2.25, stroke = NA,
             alpha = 1, shape = 19) +
  scale_x_continuous(labels = 0:10,
                       breaks = c((0:10)/10)) +
  scale_y_continuous(breaks = c(-10:0)/10,
                     labels = -10:0) +
  scale_color_manual(values = colors) +
  ylab("RCI Prediction for\nthe Challenger Party\n") +
  xlab("\nRCI for the CAQ\n") +
  labs(caption = "Bars crossing the prediction points represent 95% and 99% confidence intervals.\nPredictions for a francophone woman aged 55 or older from the Montérégie region with a collegial education.") +
  clessnverse::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))
  
ggsave("_SharedFolder_article_pot-growth/graphs/paper/7_potgrowth_estimate_individual.png",
       width = 10, height = 5)


