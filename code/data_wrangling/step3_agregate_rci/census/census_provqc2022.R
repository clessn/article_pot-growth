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

get_riding_df <- function(riding_id = 648){
  
}
