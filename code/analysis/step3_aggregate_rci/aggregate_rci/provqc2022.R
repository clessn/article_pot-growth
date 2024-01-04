# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
Data <- readRDS("code/analysis/step3_aggregate_rci/generate_models/models/disaggregated_potgrowth_votesol.rds")

# Aggregate ---------------------------------------------------------------

# 1. Weighted mean of estimates by party-riding
# 2. Weighted aggregated standard error of estimates
# 3. Margin of error at 95% confidence level


## estimate sd
estim_sd <- function(percentiles_25, percentiles_75){
  iqr <- percentiles_75 - percentiles_25
  estimated_sd <- iqr / 1.349
  return(estimated_sd)
}

Agg <- Data %>% 
  mutate(std.error = estim_sd(conf.low, conf.high),
         weighted_estimate = estimate * prct,
         weighted_stderr = std.error^2 * prct^2) %>% 
  group_by(riding_id, party, model) %>% 
  summarise(weighted_mean_estimate = sum(weighted_estimate) / sum(prct),
            weighted_stderr = sqrt(sum(weighted_stderr) / sum(prct)^2 )) %>% 
  mutate(margin_error_95 = 1.96 * weighted_stderr,
         conf_low = weighted_mean_estimate - margin_error_95,
         conf_high = weighted_mean_estimate + margin_error_95)

saveRDS(Agg, "code/analysis/step3_aggregate_rci/generate_models/models/aggregated_potgrowth_votesol.rds")
