# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
ridings_df <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>%
  filter(level == "prov2022") %>% 
  select(riding_id, granular)

data <- rbind(readRDS("_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/prov_2023/quorum_mcq_pilote.rds"),
              readRDS("_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/rcis_prov.rds") %>% select(-female)) %>% 
  left_join(., ridings_df, by = "riding_id") %>% 
  filter(rci_CAQ >= 0) %>% 
  ## we want to test the rci as a numeric variable and as an unordered factor 
  mutate(rci_CAQ_factor = factor(rci_CAQ, ordered = FALSE),
         rci_CAQ_factor_ordered = factor(rci_CAQ, ordered = TRUE))

# Models ------------------------------------------------------------------


for (i in c("PQ", "PLQ", "QS", "PCQ")){
  for (j in unique(data$source_id)){
    mdata <- data %>% 
      filter(source_id == j)
    mdata$vd <- mdata[[paste0("rci_", i)]]
    model <- lm(vd ~ rci_CAQ_factor + granular + age + langue + male,
                       data = mdata)
    predsj <- marginaleffects::predictions(model = model,
                                                 newdata = marginaleffects::datagrid(rci_CAQ_factor = unique(mdata$rci_CAQ_factor))) %>% 
      mutate(rci_CAQ = as.numeric(as.character(rci_CAQ_factor)),
             source_id = j) %>% 
      select(source_id, rci_CAQ, estimate, conf.low, conf.high)
    print(j)
    if (j == unique(data$source_id)[1]){
      predsi <- predsj
    } else {
      predsi <- rbind(predsi, predsj)
    }
  }
  if (i == "PQ"){
    preds_df <- predsi %>% 
      mutate(party = i)
  } else {
    preds_df <- rbind(preds_df, predsi %>% mutate(party = i))
  }
  print(i)
}

survey_month_year <- c(
  "quorum_mcq_pilote" = "Summer 23",
  "datagotchi_2022_pilote1" = "July 22",
  "datagotchi_2022_pilote2" = "Aug 22",
  "omnibus_april" = "Apr 22",
  "omnibus_february" = "Feb 22",
  "omnibus_january" = "Jan 22",
  "omnibus_june" = "June 22",
  "omnibus_march" = "Mar 22",
  "omnibus_may" = "May 22"
)

preds_df$xtime <- survey_month_year[preds_df$source_id]
preds_df$xtime <- factor(preds_df$xtime, levels = c("Jan 22", "Feb 22", "Mar 22", "Apr 22", "May 22", "June 22", "July 22", "Aug 22", "Summer 23"))

# Graph -------------------------------------------------------------------

colors <- c("QLP" = "#ED1A2D", "PQ" = "#004C9D", "QS" = "#FF5605", "CPQ" = "purple")

preds_df %>% 
  filter(rci_CAQ <= 0.5) %>% 
  mutate(party = ifelse(party == "PLQ", "QLP", party),
         party = ifelse(party == "PCQ", "CPQ", party),
         party = factor(party, levels = c("QLP", "QS", "PQ", "CPQ")),
         rci_CAQ = paste0("RCI CAQ = ", rci_CAQ*10),
         rci_CAQ = factor(rci_CAQ, levels = paste0("RCI CAQ = ", 0:5))) %>% 
  ggplot(aes(x = xtime, y = estimate)) +
  facet_wrap(~rci_CAQ, nrow = 1) +
  geom_line(aes(group = party, color = party),
            show.legend = FALSE,
            alpha = 0.3) +
  geom_linerange(aes(group = party, color = party,
                     ymin = conf.low, ymax = conf.high),
                 alpha = 0.5, linewidth = 1.25) +
  geom_point(aes(color = party),
             size = 2, stroke = NA,
             alpha = 1, shape = 19) +
  scale_y_continuous(breaks = c(-10:0)/10,
                     labels = -10:0) +
  scale_color_manual(values = colors) +
  ylab("RCI Prediction\n") +
  xlab("") +
  clessnverse::theme_clean_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        panel.grid.major.x = element_line(color = "grey90",
                                          linewidth = 0.2))
  
ggsave("_SharedFolder_article_pot-growth/graphs/paper/7_potgrowth_estimate_individual_by_survey.png",
       width = 12, height = 4.5)


