library(dplyr)
library(ggplot2)

# Prov --------------------------------------------------------------------
shapes_prov <- sf::read_sf(dsn = "_SharedFolder_article_pot-growth/data/lake/shapefiles/prov/Circonscription_electorale_2022_shapefile.shp") %>% 
  filter(substr(CO_CEP, 1, 1) == "3") %>% 
  arrange(CO_CEP)

nb <- spdep::poly2nb(shapes_prov, snap=1e-5)
cards <- spdep::card(nb)

maxconts <- sample(1:nrow(shapes_prov), 1) ## random montreal riding
fg <- rep("lightgrey", length(cards))
fg[maxconts] <- "red"
fg[nb[[maxconts]]] <- "green"
plot(sf::st_geometry(shapes_prov), col=fg)

# Fed ---------------------------------------------------------------------
shapes_fed <- sf::read_sf(dsn = "_SharedFolder_article_pot-growth/data/lake/shapefiles/fed/FED_CA_2021_FR/FED_CA_2021_FR.shp") %>% 
  filter(substr(NUMCF, 1, 2) == "24" &
           NUMCF != "24001")

nb <- spdep::poly2nb(shapes_fed, snap=1e-5)
cards <- spdep::card(nb)

maxconts <- sample(1:nrow(shapes_fed), 1) ## random montreal riding
fg <- rep("lightgrey", length(cards))
fg[maxconts] <- "red"
fg[nb[[maxconts]]] <- "green"
plot(sf::st_geometry(shapes_fed), col=fg)
