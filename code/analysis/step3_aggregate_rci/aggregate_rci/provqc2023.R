# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2023/disaggregated/potgrowth_votesolidity.rds")

# Aggregate ---------------------------------------------------------------

# 1. Weighted mean of estimates by party-riding
# 2. Weighted aggregated standard error of estimates
# 3. Margin of error at 95% confidence level

Agg <- Data %>% 
  mutate(weighted_estimate = estimate * prct,
         weighted_stderr = std.error^2 * prct^2) %>% 
  group_by(riding_id, party, model) %>% 
  summarise(weighted_mean_estimate = sum(weighted_estimate) / sum(prct),
            weighted_stderr = sqrt(sum(weighted_stderr) / sum(prct)^2 )) %>% 
  mutate(margin_error_95 = 1.96 * weighted_stderr,
         conf_low = weighted_mean_estimate - margin_error_95,
         conf_high = weighted_mean_estimate + margin_error_95)

saveRDS(Agg, "_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2023/aggregated/potgrowth_votesolidity.rds")

