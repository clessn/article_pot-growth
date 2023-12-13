# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
Raw <- readxl::read_excel("_SharedFolder_article_pot-growth/data/lake/census/provqc2022/statistiques-recensement-2021-CEP.xls",
                          sheet = "125 CEP 2022")

row1 <- unlist(Raw[1,])
row1[is.na(row1)] <- ""
### paste row 1 in column name
names(Raw) <- paste0(names(Raw), row1)

# Associate riding names to their codes -----------------------------------
raw_riding_names <- names(Raw)[-c(1:3)]

prov_ridings <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/dimensions/prov_ridings/data.rds")

clean_riding_names <- prov_ridings$riding_name[stringdist::amatch(x = raw_riding_names, table = prov_ridings$riding_name, maxDist = 2)]
df <- data.frame(raw_riding_names, clean_riding_names) ## check if worked
## worked, associate names in Raw to their riding_ids

riding_ids <- prov_ridings$riding_id[stringdist::amatch(x = raw_riding_names, table = prov_ridings$riding_name, maxDist = 2)]
names(riding_ids) <- raw_riding_names

# Wrangling ---------------------------------------------------------------

get_value <- function(riding_id = 648, characteristic){
  riding_column <- names(riding_ids)[riding_ids == riding_id]
  value <- Raw[[riding_column]][which(Raw$characteristic == characteristic)]
  return(as.numeric(value))
}


get_riding_df <- function(riding_id = 648){
  riding_column <- names(riding_ids)[riding_ids == riding_id]
  total_pop <- get_value(riding_id = riding_id,
                         characteristic = "Population totale 2021 (Données intégrales)")
  pop_014 <- get_value(riding_id = riding_id,
                       characteristic = "0 à 14 ans (nombre)")
  total_pop14p <- total_pop - pop_014
  age1834 <-  get_value(riding_id = riding_id,
                        characteristic = "15 à 29 ans  (nombre)")
  age3455 <-  get_value(riding_id = riding_id,
                        characteristic = "30 à 44 ans  (nombre)") +
              get_value(riding_id = riding_id,
                        characteristic = "45 à 59 ans  (nombre)")
  age55p <-  get_value(riding_id = riding_id,
                        characteristic = "60 à 74 ans  (nombre)") +
             get_value(riding_id = riding_id,
                       characteristic = "75 et plus  (nombre)")
  langue_df <- Raw[276:286,]
  french <- langue_df[[riding_column]][which(langue_df$characteristic == "Français (nombre)")]
  english <- langue_df[[riding_column]][which(langue_df$characteristic == "Anglais (nombre)")]
  other <- langue_df[[riding_column]][which(langue_df$characteristic == "Langues non officielles (nombre)")]
  df <- data.frame(
    riding_id = riding_id,
    level = "provqc2022",
    var = c(rep("gender", 2), rep("age", 3), rep("langue", 3)),
    category = c("men", "women", "1834", "3554", "55p", "french", "english", "other"),
    n = c(
      get_value(riding_id = riding_id, characteristic = "Hommes + (nombre)"), # men
      get_value(riding_id = riding_id, characteristic = "Femmes + (nombre)"), # women
      age1834,
      age3455,
      age55p,
      french,
      english,
      other
    ),
    total_pop = total_pop,
    total_pop14p = total_pop14p
  )
}

t <- get_riding_df()


for (i in 1:length(riding_ids)){
  riding_idi <- riding_ids[i]
  if (i == 1){
    Clean <- get_riding_df(riding_idi)
  } else {
    Clean <- rbind(Clean, get_riding_df(riding_idi))
  }
  print(i)
  print(riding_idi)
}

saveRDS(Clean, "_SharedFolder_article_pot-growth/data/warehouse/dimensions/census/provqc2022/census.rds")

