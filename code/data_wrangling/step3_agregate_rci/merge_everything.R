# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
path_to_data <- "_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci"
path_to_prov <- file.path(path_to_data, "separated_prov")
path_to_fed <- file.path(path_to_data, "separated_fed")


# Prov --------------------------------------------------------------------

files <- list.files(path = path_to_prov,
                    full.names = TRUE)

for (i in files){
  datai <- readRDS(i)
  if (i == files[1]){
    data_prov <- datai
  } else {
    data_prov <- rbind(data_prov, datai)
  }
  print(i)
}

saveRDS(data_prov, file.path(path_to_data, "rcis_prov.rds"))


# Fed ---------------------------------------------------------------------

files <- list.files(path = path_to_fed,
                    full.names = TRUE)

for (i in files){
  datai <- readRDS(i)
  if (i == files[1]){
    data_fed <- datai
  } else {
    data_fed <- rbind(data_fed, datai)
  }
  print(i)
}

saveRDS(data_fed, file.path(path_to_data, "rcis_fed.rds"))

