# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
Data <- read.csv("_SharedFolder_article_pot-growth/data/lake/resultats_elxn2021/EventResults.csv",
                  sep = "\t") %>% 
  filter(Type.of.results. == "validated") %>% 
  rename(riding_id = "Electoral.district.number...Numéro.de.la.circonscription",
         riding_name = "Electoral.district.name",
         party = "Political.affiliation",
         nvotes_party = "Votes.obtained...Votes.obtenus",
         nvotes_rejected = "Rejected.ballots...Bulletins.rejetés...",
         nvotes_total = "Total.number.of.ballots.cast...Nombre.total.de.votes.déposés") %>% 
  mutate(nvotes_total = nvotes_total - nvotes_rejected,
         prop_vote = nvotes_party / nvotes_total,
         party = case_when(
           party == "Conservative" ~ "PCC",
           party == "Liberal" ~ "PLC",
           party == "People's Party - PPC" ~ "PPC",
           party == "NDP-New Democratic Party" ~ "NPD",
           party == "Bloc Québécois" ~ "BQ",
           party == "Green Party" ~ "PVC" 
         ),
         year = 2021,
         level = "fed_can") %>%
  drop_na(party) %>% 
  select(year,
         level,
         riding_id,
         riding_name,
         party,
         nvotes_party,
         nvotes_total,
         prop_vote)

saveRDS(Data, "_SharedFolder_article_pot-growth/data/warehouse/step2_electoral_swings/results_fedcan2021.rds")  

