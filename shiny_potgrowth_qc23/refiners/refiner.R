# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
PotGrowth <- readRDS("shiny_potgrowth_qc23/PotGrowthQc2324/data/warehouse/provqc2023/disaggregated/potgrowth_votesolidity.rds") %>% 
  filter(model == "potgrowth") %>% 
  mutate(estimate = ifelse(estimate < -1, -1, estimate),
         estimate = ifelse(estimate > 0, 0, estimate),
         estimate = round(estimate, 2))

VoteInt <- readRDS("shiny_potgrowth_qc23/PotGrowthQc2324/data/warehouse/provqc2023/disaggregated/voteint.rds")

Data <- left_join(PotGrowth, VoteInt, by = c("male", "age", "langue", "riding_id", "party"))

riding_names_df <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/dimensions/prov_ridings/data.rds") %>% 
  select(riding_id, riding_name)

# Wrangling ---------------------------------------------------------------

### relativize parties' estimates for each profile
by_profile <- Data %>% 
  group_by(male, age, langue, riding_id) %>% 
  mutate(
    ## rank of party within this group
    rank = rank(-estimate, ties.method = "last"),
    ## adjusted pot growth estimate within this group
    ## when taking into account the rank of the party,
    ## how many parties to takeover. In short, the adj_estimate
    ## will defavorise low ranking parties as they need to takeover
    ## more parties. Do +5 (the lowest possible value) so that everything is positive.
    ## then divide by 5 to have between 0 and 1
    adj_estimate = ((max(estimate) - (max(estimate) - estimate) * rank) + 5) / 5,
    ## multiply the adjusted estimate with the voters from the profile not actually
    ## voting for the party
    anti_vote_share = 1 - predicted_vote_share,
    ## give 3x more weight to adj_estimate
    potgrowth = adj_estimate^3 * anti_vote_share) %>% 
  left_join(., riding_names_df, by = "riding_id")

saveRDS(by_profile, "shiny_potgrowth_qc23/PotGrowthQc2324/data/marts/by_profile.rds")


#### Aggregating by riding

by_riding_survey <- by_profile %>% 
  mutate(w_adj_estimate = adj_estimate * prct,
         w_antivoteshare = anti_vote_share * prct,
         w_potgrowth = potgrowth * prct,
         weighted_stderr = std.error^2 * prct^2) %>% 
  group_by(riding_id, granular, party) %>% 
  summarise(adj_estimate = sum(w_adj_estimate) / sum(prct),
            anti_vote_share = sum(w_antivoteshare) / sum(prct),
            potgrowth = sum(w_potgrowth) / sum(prct),
            weighted_stderr = sqrt(sum(weighted_stderr) / sum(prct)^2 )) %>% 
  mutate(margin_error_95 = 1.96 * weighted_stderr,
         model_proj_vote = "surveydata")

## redo using qc125
qc125 <- readRDS("shiny_potgrowth_qc23/PotGrowthQc2324/data/warehouse/qc125_provqc2022.rds") %>% 
  select(-margin)

by_riding_qc125 <- by_riding_survey %>% 
  left_join(., qc125, by = c("riding_id", "party")) %>% 
  mutate(model_proj_vote = "qc125",
         anti_vote_share = 1 - proj_vote,
         ## give 3x more weight to adj_estimate
         potgrowth = adj_estimate^3 * anti_vote_share) %>% 
  select(-proj_vote)

by_riding <- rbind(by_riding_survey, by_riding_qc125) %>% 
  left_join(., riding_names_df, by = "riding_id")

saveRDS(by_riding, "shiny_potgrowth_qc23/PotGrowthQc2324/data/marts/by_riding.rds")
