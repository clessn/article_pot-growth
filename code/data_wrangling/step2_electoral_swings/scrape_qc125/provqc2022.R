# Packages ----------------------------------------------------------------
library(dplyr)
library(rvest)
library(xml2)

# Data --------------------------------------------------------------------
ridings <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  filter(level == "prov2022")

party_qc125_colors <- c(
  "#12BBFF" = "CAQ",
  "#d90000" = "PLQ",
  "#ff5402" = "QS",
  "#0101CC" = "PQ",
  "#172853" = "PCQ"
)

# Code to scrape ----------------------------------------------------------------

for (i in 1:125){
  text <- rvest::read_html(paste0("https://qc125.com/", as.character(1000 + i), "f.htm")) %>% 
    rvest::html_nodes("svg text")
  riding_name <- text[[2]] %>% html_text()
  riding_id <- ridings$riding_id[stringdist::amatch(riding_name, ridings$riding_name, maxDist = 2)]
  for (j in 3:7){
    party <- unname(party_qc125_colors[xml_attrs(text[[j]])["fill"]])
    text_proj_vote <- strsplit(text[[j]] %>% html_text(), split = " ")
    proj_vote <- as.numeric(gsub("[^0-9]", "", text_proj_vote[[1]][1]))/100
    margin <- as.numeric(gsub("[^0-9]", "", text_proj_vote[[1]][3]))/100
    if (j == 3){
      qc125i <- data.frame(
        riding_id = riding_id,
        party = party,
        proj_vote = proj_vote,
        margin = margin
      )
    } else {
      qc125i <- rbind(qc125i, data.frame(
        riding_id = riding_id,
        party = party,
        proj_vote = proj_vote,
        margin = margin
      ))
    }
  }
  if (i == 1){
    qc125 <- qc125i
  } else {
    qc125 <- rbind(qc125, qc125i)
  }
  message(paste0(i, " - ", riding_id, "-", riding_name))
}

clessnverse::count_na(qc125$riding_id)

saveRDS(qc125, "_SharedFolder_article_pot-growth/data/warehouse/step2_electoral_swings/qc125_provqc2022.rds")
