# Packages ----------------------------------------------------------------
library(dplyr)
library(rvest)
library(xml2)

# Data --------------------------------------------------------------------
ridings <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  filter(level == "fed2021" &
           substr(riding_id, 3, 5) != "999")

riding_ids <- unique(ridings$riding_id)

party_338can_colors <- c(
  "#0202ff" = "PCC",
  "#d90000" = "PLC",
  "#E17C0D" = "NPD",
  "#12bbff" = "BQ",
  "#269b26" = "PVC"
)

# Code to scrape ----------------------------------------------------------------

for (i in 1:length(riding_ids)){
  riding_idi <- riding_ids[i]
  text <- rvest::read_html(paste0("https://338canada.com/", riding_idi, "e.htm")) %>% 
    rvest::html_nodes("svg text")
  riding_name <- text[[2]] %>% html_text()
  for (j in 3:7){
    party <- unname(party_338can_colors[xml_attrs(text[[j]])["fill"]])
    text_proj_vote <- strsplit(text[[j]] %>% html_text(), split = " ")
    proj_vote <- as.numeric(gsub("[^0-9]", "", text_proj_vote[[1]][1]))/100
    margin <- as.numeric(gsub("[^0-9]", "", text_proj_vote[[1]][3]))/100
    if (j == 3){
      can338i <- data.frame(
        riding_id = riding_idi,
        party = party,
        proj_vote = proj_vote,
        margin = margin,
        j = j
      )
    } else {
      can338i <- rbind(can338i, data.frame(
        riding_id = riding_idi,
        party = party,
        proj_vote = proj_vote,
        margin = margin,
        j = j
      ))
    }
  }
  if (i == 1){
    can338 <- can338i
  } else {
    can338 <- rbind(can338, can338i) %>% 
      tidyr::drop_na()
  }
  message(paste0(i, " - ", riding_idi, "-", riding_name))
}

clessnverse::count_na(can338$riding_id)

saveRDS(can338, "_SharedFolder_article_pot-growth/data/warehouse/step2_electoral_swings/can338_fedcan2021.rds")
