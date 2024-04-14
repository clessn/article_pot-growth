# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
ridings_df <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>%
  filter(level == "prov2022") %>% 
  select(riding_id, granular)

data <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/rcis_prov.rds") %>% 
  left_join(., ridings_df, by = "riding_id") %>% 
  ## we want to test the rci as a numeric variable and as an unordered factor 
  #mutate(rci_CAQ_factor = factor(rci_CAQ, ordered = FALSE),
  #       rci_CAQ_factor_ordered = factor(rci_CAQ, ordered = TRUE)) %>% 
  rename(rci_QLP = rci_PLQ,
         rci_CPQ = rci_PCQ) %>% 
  select(-female) %>% 
  tidyr::drop_na()

# Models ------------------------------------------------------------------

colors <- c("CAQ" = "#00cccc", "QLP" = "#ED1A2D", "PQ" = "#004C9D", "QS" = "#FF5605", "CPQ" = "purple")
parties <- names(colors)

for (i in parties){
  mdata <- data
  mdata$vi <- data[[paste0("rci_", i)]]
  mdata <- mdata %>% 
    filter(vi >= 0)
  mdata$vi_factor <- factor(mdata$vi, ordered = FALSE)
  partiesj <- parties[parties != i]
  message(i)
  for (j in partiesj){
    mdata$vd <- mdata[[paste0("rci_", j)]]
    model_num <- lm(vd ~ vi + granular + age + langue + male + educ,
                    data = mdata)
    model_factor <- lm(vd ~ vi_factor + granular + age + langue + male + educ,
                       data = mdata)
    preds_num <- marginaleffects::predictions(model = model_num,
                                              newdata = marginaleffects::datagrid(vi = (0:10)/10)) %>% 
      mutate(rci_class = "numeric",
             party_vi = i,
             party_vd = j) %>%
      select(rci_class, party_vi, rci_vi = vi, party_vd, rci_vd = estimate, conf.low, conf.high)
    preds_factor <- marginaleffects::predictions(model = model_factor,
                                                 newdata = marginaleffects::datagrid(vi_factor = unique(mdata$vi_factor))) %>% 
      mutate(rci_class = "factor",
             rci_vi = as.numeric(as.character(vi_factor)),
             party_vi = i,
             party_vd = j) %>% 
      select(rci_class, party_vi, rci_vi, party_vd, rci_vd = estimate, conf.low, conf.high)
    predsj <- rbind(preds_num, preds_factor)
    if (j == partiesj[1]){
      predsi <- predsj
  } else {
      predsi <- rbind(predsi, predsj)
  }
    message(paste0("  - ", j))
  }
  if (i == parties[1]){
    preds_df <- predsi
  } else {
    preds_df <- rbind(preds_df, predsi)
  }
}

# Graph -------------------------------------------------------------------

for (i in parties){
  preds_df %>% 
    filter(party_vi == i) %>%
    mutate(rci_class = case_when(
      rci_class == "factor" ~ paste0("RCI for ", i, ":\nCategorical"),
      rci_class == "numeric" ~ paste0("RCI for ", i, ":\nNumerical")
    )) %>% 
    ggplot(aes(x = rci_vi, y = rci_vd)) +
    facet_wrap(~rci_class) +
    geom_line(aes(group = party_vd, color = party_vd),
              show.legend = FALSE,
              alpha = 0.3) +
    geom_linerange(aes(group = party_vd, color = party_vd,
                       ymin = conf.low, ymax = conf.high),
                   alpha = 0.5, linewidth = 1.25) +
    geom_point(aes(color = party_vd),
               size = 2, stroke = NA,
               alpha = 1, shape = 19) +
    scale_x_continuous(labels = 0:10,
                       breaks = c((0:10)/10)) +
    scale_y_continuous(breaks = c(-10:0)/10,
                       labels = -10:0) +
    scale_color_manual(values = colors) +
    ylab("RCI Prediction\n") +
    xlab(paste0("\nRCI for ", i, "\n")) +
    clessnverse::theme_clean_light() +
    theme(axis.title.x = element_text(hjust = 0.5),
          axis.title.y = element_text(hjust = 0.5))
  ggsave(paste0("_SharedFolder_article_pot-growth/graphs/paper/by_party/party_voters/", i, ".png"),
         width = 9, height = 4.5)
}


# With party non-voters too -------------------------------------------------

for (i in parties){
  mdata <- data
  mdata$vi <- data[[paste0("rci_", i)]]
  mdata$vi_factor <- factor(mdata$vi, ordered = FALSE)
  partiesj <- parties[parties != i]
  message(i)
  for (j in partiesj){
    mdata$vd <- mdata[[paste0("rci_", j)]]
    model_num <- lm(vd ~ vi + granular + age + langue + male + educ,
                    data = mdata)
    model_factor <- lm(vd ~ vi_factor + granular + age + langue + male + educ,
                       data = mdata)
    preds_num <- marginaleffects::predictions(model = model_num,
                                              newdata = marginaleffects::datagrid(vi = (-10:10)/10)) %>% 
      mutate(rci_class = "numeric",
             party_vi = i,
             party_vd = j) %>%
      select(rci_class, party_vi, rci_vi = vi, party_vd, rci_vd = estimate, conf.low, conf.high)
    preds_factor <- marginaleffects::predictions(model = model_factor,
                                                 newdata = marginaleffects::datagrid(vi_factor = unique(mdata$vi_factor))) %>% 
      mutate(rci_class = "factor",
             rci_vi = as.numeric(as.character(vi_factor)),
             party_vi = i,
             party_vd = j) %>% 
      select(rci_class, party_vi, rci_vi, party_vd, rci_vd = estimate, conf.low, conf.high)
    predsj <- rbind(preds_num, preds_factor)
    if (j == partiesj[1]){
      predsi <- predsj
    } else {
      predsi <- rbind(predsi, predsj)
    }
    message(paste0("  - ", j))
  }
  if (i == parties[1]){
    preds_df <- predsi
  } else {
    preds_df <- rbind(preds_df, predsi)
  }
}


# Graph -------------------------------------------------------------------

for (i in parties){
  preds_df %>% 
    filter(party_vi == i &
             rci_class == "factor") %>%
    ggplot(aes(x = rci_vi, y = rci_vd)) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_line(aes(group = party_vd, color = party_vd),
              show.legend = FALSE,
              alpha = 0.3) +
    geom_linerange(aes(group = party_vd, color = party_vd,
                       ymin = conf.low, ymax = conf.high),
                   alpha = 0.5, linewidth = 1.25) +
    geom_point(aes(color = party_vd),
               size = 2, stroke = NA,
               alpha = 1, shape = 19) +
    scale_x_continuous(labels = -10:10,
                       breaks = c((-10:10)/10)) +
    scale_y_continuous(breaks = c(-10:10)/10,
                       labels = -10:10) +
    scale_color_manual(values = colors) +
    ylab("RCI Prediction\n") +
    xlab(paste0("\nRCI for ", i, "\n")) +
    clessnverse::theme_clean_light() +
    theme(axis.title.x = element_text(hjust = 0.5),
          axis.title.y = element_text(hjust = 0.5))
  ggsave(paste0("_SharedFolder_article_pot-growth/graphs/paper/by_party/", i, ".png"),
         width = 9, height = 4.5)
}


