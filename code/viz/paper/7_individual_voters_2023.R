# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
ridings_df <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>%
  filter(level == "prov2022") %>% 
  select(riding_id, granular)

data <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/prov_2023/quorum_mcq_pilote.rds") %>% 
  left_join(., ridings_df, by = "riding_id") %>% 
  filter(rci_CAQ >= 0) %>% 
  ## we want to test the rci as a numeric variable and as an unordered factor 
  mutate(rci_CAQ_factor = factor(rci_CAQ, ordered = FALSE),
         rci_CAQ_factor_ordered = factor(rci_CAQ, ordered = TRUE))

# Models ------------------------------------------------------------------

for (i in c("PQ", "PLQ", "QS", "PCQ")){
  mdata <- data
  mdata$vd <- data[[paste0("rci_", i)]]
  model_num <- lm(vd ~ rci_CAQ + granular + age + langue + male,
                  data = mdata)
  model_factor <- lm(vd ~ rci_CAQ_factor + granular + age + langue + male,
                     data = mdata)
  model_factor_ord <- lm(vd ~ rci_CAQ_factor_ordered + granular + age + langue + male,
                     data = mdata)
  preds_num <- marginaleffects::predictions(model = model_num,
                                            newdata = marginaleffects::datagrid(rci_CAQ = (0:10)/10)) %>% 
    mutate(rci_class = "numeric") %>%
    select(rci_class, rci_CAQ, estimate, conf.low, conf.high)
  preds_factor <- marginaleffects::predictions(model = model_factor,
                                               newdata = marginaleffects::datagrid(rci_CAQ_factor = unique(mdata$rci_CAQ_factor))) %>% 
    mutate(rci_class = "factor",
           rci_CAQ = as.numeric(as.character(rci_CAQ_factor))) %>% 
    select(rci_class, rci_CAQ, estimate, conf.low, conf.high)
  preds_factor_ord <- marginaleffects::predictions(model = model_factor_ord,
                                                    newdata = marginaleffects::datagrid(rci_CAQ_factor_ordered = unique(mdata$rci_CAQ_factor_ordered))) %>% 
    mutate(rci_class = "factor_ordered",
           rci_CAQ = as.numeric(as.character(rci_CAQ_factor_ordered))) %>% 
    select(rci_class, rci_CAQ, estimate, conf.low, conf.high)
  predsi <- rbind(preds_num, preds_factor, preds_factor_ord) %>% 
    mutate(party = i)
  if (i == "PQ"){
    preds_df <- predsi
  } else {
    preds_df <- rbind(preds_df, predsi)
  }
}


# Graph -------------------------------------------------------------------

colors <- c("QLP" = "#ED1A2D", "PQ" = "#004C9D", "QS" = "#FF5605", "CPQ" = "purple")

preds_df %>% 
  filter(rci_class != "factor_ordered") %>% 
  mutate(party = ifelse(party == "PLQ", "QLP", party),
         party = ifelse(party == "PCQ", "CPQ", party),
         party = factor(party, levels = c("QLP", "QS", "PQ", "CPQ")),
         rci_class = case_when(
           rci_class == "factor" ~ "RCI for the CAQ:\nCategorical",
           rci_class == "factor_ordered" ~ "RCI for the CAQ:\nOrdered category",
           rci_class == "numeric" ~ "RCI for the CAQ:\nNumerical"
         )) %>% 
  ggplot(aes(x = rci_CAQ, y = estimate)) +
  facet_wrap(~rci_class) +
  geom_line(aes(group = party, color = party),
            show.legend = FALSE,
            alpha = 0.3) +
  geom_linerange(aes(group = party, color = party,
                     ymin = conf.low, ymax = conf.high),
                 alpha = 0.5, linewidth = 1.25) +
  geom_point(aes(color = party),
             size = 2, stroke = NA,
             alpha = 1, shape = 19) +
  scale_x_continuous(labels = 0:10,
                       breaks = c((0:10)/10)) +
  scale_y_continuous(breaks = c(-10:0)/10,
                     labels = -10:0) +
  scale_color_manual(values = colors) +
  ylab("RCI Prediction\n") +
  xlab("\nRCI for the CAQ\n") +
  clessnverse::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))
  
ggsave("_SharedFolder_article_pot-growth/graphs/paper/7_potgrowth_estimate_individual_2023.png",
       width = 9, height = 4.5)


