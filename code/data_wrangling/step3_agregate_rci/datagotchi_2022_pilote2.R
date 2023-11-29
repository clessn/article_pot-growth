# Load packages ----------------------------------------------------------------
library(dplyr)

# Load raw data -----------------------------------------------------------

### Remove respondents who didnt answer RCI ---------------------------------

all_respondents_df <- sondr::read_any_csv("_SharedFolder_article_pot-growth/data/lake/datagotchi_2022_pilote2/datagotchi_pilot2_2022.csv")
## this df contains all 1970 respondents of this survey, but with the RCI cleaned.

#### for the RCI
####### if the respondent has answered NA to 4 or less parties,
######### this means that the NAs should become 0.5, the default position of the slider in Qualtrics
######### (programming error in Qualtrics)
raw1 <- readRDS("_SharedFolder_article_pot-growth/data/lake/datagotchi_2022_pilote2/Pilote2.rds")
raw1$nas <- rowSums(is.na(raw1 %>% select(starts_with("potGrowth"))))
table(raw1$nas)
# 0 means the respondent answered for the 5 parties (nothing to do)
# 1,2,3 or 4 means the respondent answered the question but skipped some parties
##### (which means the respondent didnt change the party from the default 5 position in Qualtrics)
# 5 means the respondent didnt answer the question

### Remove respondents who didnt answer potGrowth question
respondents_to_remove <- which(raw1$nas==5)
Raw <- all_respondents_df[-respondents_to_remove,]

# Create empty clean dataframe --------------------------------------------
Clean <- data.frame(id = 1:nrow(Raw), # id of the respondent
                    source_id = "datagotchi_2022_pilote2", # id of the survey
                    year = 2022, # year of the survey
                    level = "prov_qc") # fed_can or prov_qc

# Clean variables ---------------------------------------------------------

## gender ------------------------------------------------------------------
table(Raw$gender)
Clean$male <- NA
Clean$male[as.numeric(Raw$gender) %in% c(1,3)] <- 1
Clean$male[as.numeric(Raw$gender) %in% c(2,4,5,6,7,8)] <- 0
table(Clean$male)

table(Raw$gender)
Clean$female <- NA
Clean$female[as.numeric(Raw$gender) %in% c(2,4)] <- 1
Clean$female[as.numeric(Raw$gender) %in% c(1,3,5,6,7,8)] <- 0
table(Clean$female)


## age ------------------------------------------------------------------
table(Raw$age)

## language ------------------------------------------------------------------

## riding ------------------------------------------------------------------

#### Load data from article_riding_volatility to get riding
riding_volatility_df <- readRDS("_SharedFolder_article_pot-growth/data/lake/riding_volatility_data.rds") %>% 
  # filter for datagotchi_2022_pilote2 only
  filter(source_id == "pilote2")
table(riding_volatility_df$riding_id)

## Create riding_id column in Clean
Clean$riding_id <- riding_volatility_df$riding_id

## Load riding names (to join on riding_id)
riding_names_df <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/dimensions/prov_ridings/data.rds") %>% 
  select(riding_id, riding_name)

### Join riding_name on riding_id
Clean <- left_join(Clean, riding_names_df, by = "riding_id")

## RCI ---------------------------------------------------------------------


#### we need to take the rci data from all_respondents_df, I think.

### Replace NAs with 0.5
Raw <- Raw %>% 
  tidyr::replace_na(list(potGrowthCAQ=0.5,
                         potGrowthPLQ=0.5,
                         potGrowthQS=0.5,
                         potGrowthPQ=0.5,
                         potGrowthPCQ=0.5))


# Save Clean to a rds dataset ---------------------------------------------

saveRDS(Clean, "_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/separated_prov/datagotchi_2022_pilote2.rds")
