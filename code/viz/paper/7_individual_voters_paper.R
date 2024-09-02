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
  mutate(rci_CAQ = factor(rci_CAQ, ordered = FALSE)) %>% 
  rename(rci_QLP = rci_PLQ,
         rci_CPQ = rci_PCQ) %>% 
  select(-female) %>% 
  tidyr::drop_na()

# With the RCI ------------------------------------------------------------------

for (i in c("PQ", "QLP", "QS", "CPQ")){
  mdata <- data
  mdata$vd <- data[[paste0("rci_", i)]]
  model <- lm(vd ~ rci_CAQ + granular + age + langue + male + educ,
                  data = mdata)
  predsi <- marginaleffects::predictions(model = model,
                                               newdata = marginaleffects::datagrid(rci_CAQ = unique(mdata$rci_CAQ)),
                                               conf_level = 0.95) %>%
    mutate(conf_level = "95") %>%
    rbind(marginaleffects::predictions(model = model,
                                       newdata = marginaleffects::datagrid(rci_CAQ = unique(mdata$rci_CAQ)),
                                       conf_level = 0.99) %>% 
            mutate(conf_level = "99")) %>%
    mutate(rci_CAQ = as.numeric(as.character(rci_CAQ))) %>% 
    select(rci_CAQ, estimate, conf.low, conf.high, conf_level) %>% 
    mutate(party = i) %>% 
    tidyr::pivot_wider(names_from = conf_level, values_from = c(conf.low, conf.high))
  if (i == "PQ"){
    preds_rci <- predsi
  } else {
    preds_rci <- rbind(preds_rci, predsi)
  }
  message(i)
}

## Graph -------------------------------------------------------------------

colors <- c("QLP" = "#ED1A2D", "PQ" = "#004C9D", "QS" = "#FF5605", "CPQ" = "purple")

preds_rci %>% 
  mutate(party = factor(party, levels = c("QLP", "QS", "PQ", "CPQ"))) %>% 
  ggplot(aes(x = rci_CAQ, y = estimate)) +
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
  clessnize::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))

