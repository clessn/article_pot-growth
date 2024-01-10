# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------

ResultsQc125 <- left_join(readRDS("_SharedFolder_article_pot-growth/data/warehouse/step2_electoral_swings/qc125_provqc2022.rds"),
                          readRDS("_SharedFolder_article_pot-growth/data/warehouse/step2_electoral_swings/results_provqc2022.rds"),
                          by = c("riding_id", "party")) %>% 
  mutate(delta = proj_vote - prop_vote) %>% 
  group_by(riding_id) %>% 
  mutate(winner2022 = ifelse(prop_vote == max(prop_vote), 1, 0))

ggplot(ResultsQc125, aes(x = delta)) +
  geom_histogram() +
  facet_wrap(~party) +
  geom_vline(xintercept = 0)

## aggregated
DataAgg <- readRDS("_SharedFolder_article_pot-growth/data/marts/rci_by_riding/provqc2022/aggregated/potgrowth_votesolidity.rds") %>%
  select(riding_id, party, model, estimate = weighted_mean_estimate, stderr = weighted_stderr) %>%
  left_join(., ResultsQc125, by = c("riding_id", "party")) %>% 
  filter(riding_id != "938") %>% # removing Ungava too uncertain
  tidyr::drop_na() %>% 
  mutate(conf.low = estimate - stderr*1.96,
         conf.high = estimate + stderr*1.96)

party_colors <- c(
    "CAQ" = "#12BBFF",
    "PLQ" = "#d90000",
    "QS" = "#ff5402",
    "PQ" = "#0101CC",
    "PCQ" = "#172853"
  )


# Explo aggregated ------------------------------------------------------

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
    ylab("qc125 projected vote share")

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
  ylab("qc125 projected vote share")

## gains - potgrowth
DataAgg %>% 
  mutate(confidence = 1 - stderr) %>% 
  filter(model == "potgrowth") %>%
  ggplot(aes(x = estimate, y = delta)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~party) +
  xlab("pot growth estimate") +
  ylab("Gains depuis l'élection 2022\n(Projection Qc125 - résultats électoraux 2022)")

## gains - vote solidity
DataAgg %>% 
  mutate(confidence = 1 - stderr) %>% 
  filter(model == "vote_solidity") %>%
  ggplot(aes(x = estimate, y = delta)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~party) +
  xlab("vote solidity estimate") +
  ylab("Gains depuis l'élection 2022\n(Projection Qc125 - résultats électoraux 2022)")


## gains - pot growth
DataAgg %>% 
  mutate(confidence = 1 - stderr) %>% 
  filter(model == "potgrowth") %>%
  ggplot(aes(x = estimate, y = delta)) +
  geom_point(aes(color = party)) +
  scale_color_manual(values = party_colors) +
  scale_y_continuous(limits = c(0, 0.4)) +
  xlab("pot growth estimate") +
  ylab("Gains depuis l'élection 2022\n(Projection Qc125 - résultats électoraux 2022)")

## losses - vote solidity
DataAgg %>% 
  mutate(confidence = 1 - stderr) %>% 
  filter(model == "vote_solidity") %>%
  ggplot(aes(x = estimate, y = delta)) +
  geom_point(aes(color = party)) +
  scale_color_manual(values = party_colors) +
  scale_y_continuous(limits = c(-0.3, 0)) +
  xlab("vote solidity estimate") +
  ylab("Gains depuis l'élection 2022\n(Projection Qc125 - résultats électoraux 2022)")

# CAQ vote solidity vs PQ pot growth ------------------------------------------------------------

rci_corr_plot <- function(party_potgrowth = "PQ", party_votesol = "CAQ"){
  deltas <- ResultsQc125 %>%
    filter(party == party_potgrowth) %>% 
    select(riding_id, delta)
  d <- DataAgg %>% 
    mutate(confidence = 1 - stderr) %>% 
    filter((party == party_votesol & model == "vote_solidity") |
             (party == party_potgrowth & model == "potgrowth")) %>%
    tidyr::pivot_wider(.,
                       names_from = c("model", "party"),
                       values_from = c("estimate", "confidence"),
                       id_cols = c("riding_id", "riding_name")) %>% 
    left_join(., deltas, by = "riding_id")
  d$confidence <- (d[[paste0("confidence_vote_solidity_", party_votesol)]] + d[[paste0("confidence_potgrowth_", party_potgrowth)]]) / 2
  d$potgrowth <- d[[paste0("estimate_potgrowth_", party_potgrowth)]]
  d$votesol = d[[paste0("estimate_vote_solidity_", party_votesol)]]
  plot <- d %>% 
    ggplot(aes(x = potgrowth, y = votesol)) +
    geom_jitter(aes(alpha = confidence,
                    size = confidence,
                    color = delta),
                width = 0.01, height = 0.01) +
    geom_smooth(aes(weight = confidence),
                method = "lm", color = "black") +
    scale_size_continuous(range = c(1.5, 4)) +
    scale_alpha_continuous(range = c(0.3, 1)) +
    scale_color_gradient(low = party_colors[party_votesol],
                         high = party_colors[party_potgrowth]) +
    xlab(paste0("\nPotential for growth estimate\n", party_potgrowth, "\n")) +
    ylab(paste0("\nVote solidity estimate\n", party_votesol, "\n")) +
    clessnverse::theme_clean_light() +
    guides(colour = guide_legend(title = paste0("Gains depuis les élections: ", party_potgrowth),
                                 title.position = "top"),
           size = guide_legend(title = "Certainty", title.position = "top"),
           alpha = guide_legend(title = "Certainty", title.position = "top")) +
    theme(legend.title = element_text(),
          axis.title.x = element_text(hjust = 0.5),
          axis.title.y = element_text(hjust = 0.5))
  return(plot)
}


combs <- expand.grid(party_potgrowth = names(party_colors), party_votesol = names(party_colors)) %>% 
  filter(party_potgrowth != party_votesol)


for (i in 1:nrow(combs)){
  plot <- rci_corr_plot(combs$party_potgrowth[i], combs$party_votesol[i])
  ggsave(plot = plot,
         filename = paste0("_SharedFolder_article_pot-growth/graphs/step4_compare_rci_swingness/qc125/party_by_party/", combs$party_potgrowth[i], "-", combs$party_votesol[i], ".png"),
         width = 8, height = 6)
}


# Vote sol CAQ vs Potgrowth all parties ---------------------------------------------

CAQ <- DataAgg %>% 
  filter(party == "CAQ" & model == "vote_solidity" &
           winner2022 == 1) %>% 
  ungroup() %>% 
  select(riding_id, votesolCAQ = estimate)

plot <- function(
    color_pq_only = TRUE,
    vary_size = TRUE,
    vd = c("potgrowth", "prop_vote")
    ){
  
}

## only with pq color
DataAgg %>% 
  filter(party != "CAQ" & model == "potgrowth") %>% 
  left_join(., CAQ, by = "riding_id") %>%
  tidyr::drop_na(votesolCAQ) %>% 
  mutate(delta = delta*100) %>% 
  ggplot(aes(x = votesolCAQ, y = estimate)) +
  geom_point(aes(color = party,
                 size = delta),
             alpha = 0.4, shape = 19, stroke = NA) +
  guides(color = guide_legend(title = ""),
         size = guide_legend(title = "Gains by the challenger since\n2022 (vote shares %)",
                              title.position = "top")) +
  scale_color_manual(values = c(party_colors["PQ"], "Other challenger" = "grey")) +
  #scale_color_manual(values = party_colors) +
  scale_size_continuous(range = c(0.05, 5)) +
  ylab("\nPotential for growth estimate\n") +
  xlab("\nCAQ Vote solidity estimate\n") +
  ggtitle("Potential for growth of challenger parties\nin ridings won by CAQ, the incumbent") +
  clessnverse::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        legend.title = element_text())
ggsave("_SharedFolder_article_pot-growth/graphs/step4_compare_rci_swingness/qc125/challenger_potgrowth_pq.png",
       width = 8, height = 6)

## with all challengers' colors
DataAgg %>% 
  filter(party != "CAQ" & model == "potgrowth") %>% 
  left_join(., CAQ, by = "riding_id") %>%
  tidyr::drop_na(votesolCAQ) %>% 
  mutate(delta = delta*100) %>% 
  ggplot(aes(x = votesolCAQ, y = estimate)) +
  geom_point(aes(color = party,
                 size = delta),
             alpha = 0.4, shape = 19, stroke = NA) +
  guides(color = "none",
         size = guide_legend(title = "Gains by the challenger since\n2022 (vote shares %)",
                             title.position = "top")) +
  scale_color_manual(values = party_colors) +
  scale_size_continuous(range = c(0.05, 5)) +
  ylab("\nPotential for growth estimate\n") +
  xlab("\nCAQ Vote solidity estimate\n") +
  ggtitle("Potential for growth of challenger parties\nin ridings won by CAQ, the incumbent") +
  clessnverse::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        legend.title = element_text())
ggsave("_SharedFolder_article_pot-growth/graphs/step4_compare_rci_swingness/qc125/challenger_potgrowth.png",
       width = 8, height = 6)


# Vote sol CAQ vs 22 electoral results ------------------------------------

## color all parties
DataAgg %>% 
  filter(party != "CAQ" & model == "potgrowth") %>% 
  left_join(., CAQ, by = "riding_id") %>%
  tidyr::drop_na(votesolCAQ) %>% 
  mutate(prop_vote = prop_vote*100) %>% 
  ggplot(aes(x = votesolCAQ, y = prop_vote)) +
  geom_point(aes(color = party,
                 size = delta),
             alpha = 0.6, shape = 19, stroke = NA) +
  guides(color = "none",
         size = guide_legend(title = "Gains by the challenger since\n2022 (vote shares %)",
                             title.position = "top")) +
  scale_color_manual(values = party_colors) +
  scale_size_continuous(range = c(0.05, 5)) +
  ylab("\nVote share in 2022 (%)\n") +
  xlab("\nCAQ Vote solidity estimate\n") +
  ggtitle("Vote shares of challenger parties\nin ridings won by CAQ, the incumbent") +
  clessnverse::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        legend.title = element_text())
ggsave("_SharedFolder_article_pot-growth/graphs/step4_compare_rci_swingness/qc125/challenger_onlyresults.png",
       width = 8, height = 6)

## color only pq
DataAgg %>% 
  filter(party != "CAQ" & model == "potgrowth") %>% 
  left_join(., CAQ, by = "riding_id") %>%
  tidyr::drop_na(votesolCAQ) %>% 
  mutate(prop_vote = prop_vote*100) %>% 
  ggplot(aes(x = votesolCAQ, y = prop_vote)) +
  geom_point(aes(color = party,
                 size = delta),
             alpha = 0.6, shape = 19, stroke = NA) +
  guides(color = guide_legend(title = ""),
         size = guide_legend(title = "Gains by the challenger since\n2022 (vote shares %)",
                             title.position = "top")) +
  scale_color_manual(values = c(party_colors["PQ"], "Other challenger" = "grey")) +
  scale_size_continuous(range = c(0.05, 5)) +
  ylab("\nVote share in 2022 (%)\n") +
  xlab("\nCAQ Vote solidity estimate\n") +
  ggtitle("Vote shares of challenger parties\nin ridings won by CAQ, the incumbent") +
  clessnverse::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        legend.title = element_text())
ggsave("_SharedFolder_article_pot-growth/graphs/step4_compare_rci_swingness/qc125/challenger_onlyresultspq.png",
       width = 8, height = 6)


# Vote solidity CAQ vs PLQ QS ---------------------------------------------

DataAgg %>% 
  filter(winner2022 == 1 & model == "vote_solidity") %>%
  ggplot(aes(x = estimate)) +
  facet_wrap(~party, ncol = 1) +
  geom_histogram()



