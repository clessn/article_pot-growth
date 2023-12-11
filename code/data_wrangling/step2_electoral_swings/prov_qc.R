# Packages ----------------------------------------------------------------
library(dplyr)

# Wrangle --------------------------------------------------------------------

path <- "_SharedFolder_article_pot-growth/data/lake/resultats_elxn2022/resultats-bureau-vote/" 
files <- list.files(path = path)

parties <- c("CAQ", "PLQ", "QS", "PQ", "PCQ")

for (i in 1:length(files)){
  file <- files[i]
  lines <- readLines(file.path(path, file), encoding = "latin1")
  raw <- read.csv(text = lines, header = TRUE, row.names = NULL, sep = ";")
  riding_id <- raw[1, 1]
  riding_name <- raw$Circonscription[1]
  names(raw)[which(grepl("C.A.Q..E.F.L.", names(raw)))] <- "CAQ"
  names(raw)[which(grepl("P.L.Q..Q.L.P.", names(raw)))] <- "PLQ"
  names(raw)[which(grepl("Q.S.", names(raw)))] <- "QS"
  names(raw)[which(grepl("P.V.Q..G.P.Q.", names(raw)))] <- "PVQ"
  names(raw)[which(grepl("P.Q.", names(raw)))] <- "PQ"
  names(raw)[which(grepl("P.C.Q.E.E.D.", names(raw)))] <- "PCQ"
  raw2 <- raw %>% 
    filter(Nom.des.Municipalités == "Total de la circonscription") %>% 
    select(total = B.V., any_of(parties))
  values <- unlist(raw2[1,])
  names(values) <- names(raw2)
  
  ## check if missing party
  missing <- parties[!(parties %in% names(values))]
  if (!purrr::is_empty(missing)){
    missing_values <- rep(0, length(missing))
    names(missing_values) <- missing
    values <- c(values, missing_values)
    }
  
  datai <- data.frame(
    year = 2022,
    level = "prov_qc",
    riding_id = riding_id,
    riding_name = riding_name,
    party = parties,
    nvotes_party = values[parties],
    nvotes_total = values["total"]
  ) %>% 
    replace_na(list(CAQ = 0, PQ = 0, PLQ = 0, QS = 0, PCQ = 0)) %>% 
    mutate(prop_vote = nvotes_party / nvotes_total)
  if (i == 1){
    data <- datai
  } else {
    data <- rbind(data, datai)
  }
  message(paste0(i, " - ", riding_id, " - ", riding_name))
}

rownames(data) <- 1:nrow(data)

# Save it -----------------------------------------------------------------

saveRDS(data, "_SharedFolder_article_pot-growth/data/warehouse/step2_electoral_swings/results_provqc2022.rds")
