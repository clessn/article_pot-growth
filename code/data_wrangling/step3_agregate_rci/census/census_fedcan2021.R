# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
Raw <- sondr::read_any_csv("_SharedFolder_article_pot-growth/data/lake/census/fedcan2021/98-401-X2021029_English_CSV_data.csv") %>% 
  # filter for electoral riding units
  filter(GEO_LEVEL == "Federal electoral district (2023 Representation Order)" &
         ## filter for relevant characteristcs
           CHARACTERISTIC_ID %in% c(
             8, # total
             9, # 0-14 years old
             14, # 15-19 years old
             15, # 20-24 years old
             16, # 25-29 years old
             17, # 30-34 years old
             18, # 35-39 years old
             19, # 40-44 years old
             20, # 45-49 years old
             21, # 50-54 years old
             22, # 55-59 years old
             23, # 60-64 years old
             24, # 65+
             396, ## mother tongue: english
             397, ## mother tongue: french
             398 ## mother tongue: non-official
           ))

unique(Raw$GEO_LEVEL)
unique(Raw$ALT_GEO_CODE)

list_to_fetch <- list(
  "8"  = c(variable = "population", category = "total"),
  "9"  = c(variable = "age", category =  "0_14"),
  "14" = c(variable = "age", category = "15_19"),
  "15" = c(variable = "age", category = "20_24"),
  "16" = c(variable = "age", category = "25_29"),
  "17" = c(variable = "age", category = "30_34"),
  "18" = c(variable = "age", category = "35_39"),
  "19" = c(variable = "age", category = "40_44"),
  "20" = c(variable = "age", category = "45_49"),
  "21" = c(variable = "age", category = "50_54"),
  "22" = c(variable = "age", category = "55_59"),
  "23" = c(variable = "age", category = "60_64"),
  "24" = c(variable = "age", category = "65+"),
  "396" = c(variable = "mother_tongue", category = "english"),
  "397" = c(variable = "mother_tongue", category = "french"),
  "398" = c(variable = "mother_tongue", category = "non_official")
)


# Clean -------------------------------------------------------------------

get_value <- function(riding_id, gender = c("men", "women", "both"), char_ids){
  if (gender == "men"){
    col = "C2_COUNT_MEN."
  } else if (gender == "women") {
    col = "C3_COUNT_WOMEN."
  } else {
    col = "C1_COUNT_TOTAL"
  }
  value_df <- Raw %>% 
    filter(ALT_GEO_CODE == riding_id &
             CHARACTERISTIC_ID %in% char_ids)
   return(sum(value_df[[col]])) 
}

get_riding_df <- function(riding_id = "24001"){
  men <- data.frame(
    riding_id = riding_id,
    level = "fedcan",
    gender = "men",
    var = c(rep("age", 3), rep("langue", 3)),
    category = c("1834", "3554", "55p", "french", "english", "other"),
    n = c(
      get_value(riding_id, "men", 14:17), # age 1834
      get_value(riding_id, "men", 18:21), # age 3554
      get_value(riding_id, "men", 22:24), # age 55p
      get_value(riding_id, "men", 397), # langue french
      get_value(riding_id, "men", 396), # langue english
      get_value(riding_id, "men", 398) # langue other
    ),
    total_pop = get_value(riding_id, gender = "both", 8),
    total_pop14p = get_value(riding_id, gender = "both", 8) - get_value(riding_id, gender = "both", 9)
  )
  women <- data.frame(
    riding_id = riding_id,
    level = "fedcan",
    gender = "women",
    var = c(rep("age", 3), rep("langue", 3)),
    category = c("1834", "3554", "55p", "french", "english", "other"),
    n = c(
      get_value(riding_id, "women", 14:17), # age 1834
      get_value(riding_id, "women", 18:21), # age 3554
      get_value(riding_id, "women", 22:24), # age 55p
      get_value(riding_id, "women", 397), # langue french
      get_value(riding_id, "women", 396), # langue english
      get_value(riding_id, "women", 398) # langue other
    ),
    total_pop = get_value(riding_id, gender = "both", 8),
    total_pop14p = get_value(riding_id, gender = "both", 8) - get_value(riding_id, gender = "both", 9)
  )
  return(rbind(men, women))
}

for (i in 1:length(unique(Raw$ALT_GEO_CODE))){
  riding_idi <- unique(Raw$ALT_GEO_CODE)[i]
  if (i == 1){
    Clean <- get_riding_df(riding_idi)
  } else {
    Clean <- rbind(Clean, get_riding_df(riding_idi))
  }
  print(i)
  print(riding_idi)
}

saveRDS(Clean, "_SharedFolder_article_pot-growth/data/warehouse/dimensions/census/fedcan2021/census.rds")
