# Packages -----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------

ResultsCan338 <- left_join(readRDS("_SharedFolder_article_pot-growth/data/warehouse/step2_electoral_swings/can338_fedcan2021.rds") %>% mutate(riding_id = as.character(riding_id)),
                          readRDS("_SharedFolder_article_pot-growth/data/warehouse/step2_electoral_swings/results_fedcan2021.rds"),
                          by = c("riding_id", "party")) %>% 
  mutate(delta = proj_vote - prop_vote) %>% 
  group_by(riding_id) %>% 
  mutate(winner2021 = ifelse(prop_vote == max(prop_vote), 1, 0))

ggplot(ResultsCan338, aes(x = delta)) +
  geom_histogram() +
  facet_wrap(~party) +
  geom_vline(xintercept = 0)

## aggregated
DataAgg <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/fedcan2021/aggregated/potgrowth_votesolidity.rds") %>%
  mutate(riding_id = as.character(riding_id)) %>% 
  select(riding_id, party, model, estimate = weighted_mean_estimate, stderr = weighted_stderr) %>%
  left_join(., ResultsCan338, by = c("riding_id", "party")) %>% 
  tidyr::drop_na() %>% 
  mutate(conf.low = estimate - stderr*1.96,
         conf.high = estimate + stderr*1.96)

party_colors <- c(
  "PCC" = "#0202ff",
  "PLC" = "#d90000",
  "NPD" = "#E17C0D",
  "BQ" = "#12bbff",
  "PVC" = "#269b26"
)

ridings <- read.csv("_SharedFolder_article_pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  filter(level == "fed2021" &
           substr(riding_id, 3, 5) != "999")

large_regions <- ridings$large
names(large_regions) <- ridings$riding_id


# Explo -------------------------------------------------------------------

## raw - potgrowth
DataAgg %>% 
  mutate(confidence = 1 - stderr) %>% 
  filter(model == "potgrowth") %>%
  ggplot(aes(x = estimate, y = proj_vote, alpha = confidence)) +
  geom_point() +
  geom_smooth(alpha = 1, method = "lm",
              aes(weight = confidence)) +
  facet_wrap(~party) +
  xlab("pot growth estimate") +
  ylab("can338 projected vote share")

## raw - votesol
DataAgg %>% 
  mutate(confidence = 1 - stderr) %>% 
  filter(model == "vote_solidity") %>%
  ggplot(aes(x = estimate, y = proj_vote, alpha = confidence)) +
  geom_point() +
  geom_smooth(alpha = 1, method = "lm",
              aes(weight = confidence)) +
  facet_wrap(~party) +
  xlab("vote solidity estimate") +
  ylab("can338 projected vote share")

## gains - potgrowth
DataAgg %>% 
  mutate(confidence = 1 - stderr) %>% 
  filter(model == "potgrowth") %>%
  ggplot(aes(x = estimate, y = delta)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~party) +
  xlab("pot growth estimate") +
  ylab("Gains depuis l'élection 2021\n(Projection Can338 - résultats électoraux 2021)")

## gains - votesol
DataAgg %>% 
  mutate(confidence = 1 - stderr) %>% 
  filter(model == "vote_solidity") %>%
  ggplot(aes(x = estimate, y = delta)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~party) +
  xlab("vote_solidity estimate") +
  ylab("Gains depuis l'élection 2021\n(Projection Can338 - résultats électoraux 2021)")

## gains - pot growth
DataAgg %>% 
  mutate(confidence = 1 - stderr) %>% 
  filter(model == "potgrowth") %>%
  ggplot(aes(x = estimate, y = delta)) +
  geom_point(aes(color = party)) +
  scale_color_manual(values = party_colors) +
  scale_y_continuous(limits = c(0, 0.4)) +
  xlab("pot growth estimate") +
  ylab("Gains depuis l'élection 2021\n(Projection Can338 - résultats électoraux 2021)")

## losses - vote solidity
DataAgg %>% 
  mutate(confidence = 1 - stderr) %>% 
  filter(model == "vote_solidity") %>%
  ggplot(aes(x = estimate, y = delta)) +
  geom_point(aes(color = party)) +
  scale_color_manual(values = party_colors) +
  scale_y_continuous(limits = c(-0.3, 0)) +
  xlab("vote solidity estimate") +
  ylab("Gains depuis l'élection 2021\n(Projection Can338 - résultats électoraux 2021)")


# Vote sol PLC vs pot growth other parties --------------------------------

PLC <- DataAgg %>% 
  filter(party == "PLC" & model == "vote_solidity" &
           winner2021 == 1) %>% 
  ungroup() %>% 
  select(riding_id, votesolPLC = estimate)

DataAgg$large_region <- large_regions[DataAgg$riding_id]

DataAgg %>% 
  filter(party != "PLC" & model == "potgrowth") %>% 
  left_join(., PLC, by = "riding_id") %>%
  tidyr::drop_na(votesolPLC) %>% 
  mutate(delta = delta*100) %>% 
  ggplot(aes(x = votesolPLC, y = estimate)) +
  geom_jitter(aes(color = party,
                 size = delta),
             alpha = 0.4, shape = 19, stroke = NA) +
  guides(color = "none",
         size = guide_legend(title = "Gains by the challenger since\n2021 (vote shares %)",
                             title.position = "top")) +
  scale_color_manual(values = party_colors) +
  scale_size_continuous(range = c(0.05, 7)) +
  ylab("\nPotential for growth estimate\n") +
  xlab("\nPLC Vote solidity estimate\n") +
  facet_wrap(~large_region) +
  ggtitle("Potential for growth of challenger parties\nin ridings won by PLC, the incumbent") +
  clessnverse::theme_clean_light() +
  scale_x_continuous(limits = c(0.125, 0.19)) +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        legend.title = element_text())
ggsave("_SharedFolder_article_pot-growth/graphs/step4_compare_rci_swingness/can338/challenger_potgrowth.png",
       width = 8, height = 6)

