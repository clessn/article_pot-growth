# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2022/disaggregated/potgrowth_votesolidity.rds") %>% 
  filter(riding_id == 258)


kableExtra::kable_paper(kable_input = Data,
                        html_font = "arial",
                        kable_format = "html")

knitr::kable(Data)
