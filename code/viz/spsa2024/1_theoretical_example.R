# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------

set.seed(123)
data <- data.frame(
  id = 1:1000,
  vote = c(rep("A", 450), rep("B", 370), rep("C", 180))
)

data$continuous_A <- NA
data$continuous_A[data$vote == "A"] <- sample(c("vote_solid", "vote_fragile"),
                                              length(data$continuous_A[data$vote == "A"]),
                                              prob = c(0.3, 0.7),
                                              replace = TRUE)
data$continuous_A[data$vote != "A"] <- sample(c("novote_reachable", "novote_unreach"),
                                              length(data$continuous_A[data$vote != "A"]),
                                              prob = c(0.2, 0.8),
                                              replace = TRUE)

data$continuous_B <- NA
data$continuous_B[data$vote == "B"] <- sample(c("vote_solid", "vote_fragile"),
                                              length(data$continuous_B[data$vote == "B"]),
                                              prob = c(0.65, 0.35),
                                              replace = TRUE)
data$continuous_B[data$vote != "B"] <- sample(c("novote_reachable", "novote_unreach"),
                                              length(data$continuous_B[data$vote != "B"]),
                                              prob = c(0.1, 0.9),
                                              replace = TRUE)

data$continuous_C <- NA
data$continuous_C[data$vote == "C"] <- sample(c("vote_solid", "vote_fragile"),
                                              length(data$continuous_C[data$vote == "C"]),
                                              prob = c(0.55, 0.45),
                                              replace = TRUE)
data$continuous_C[data$vote != "C"] <- sample(c("novote_reachable", "novote_unreach"),
                                              length(data$continuous_B[data$vote != "C"]),
                                              prob = c(0.7, 0.3),
                                              replace = TRUE)

colors <- c("A" = "#32CD32", "B" = "#FFD700", "C" = "#FFC0CB")

# Graph 1: only traditional -----------------------------------------------

data %>% 
  group_by(vote) %>% 
  summarise(prop = n()/nrow(.)*100) %>% 
  ggplot(aes(x = prop, y = reorder(vote, prop))) +
  geom_col(aes(fill = vote),
           color = NA, alpha = 0.35,
           show.legend = FALSE) +
  geom_text(aes(x = prop - 5,
                label = paste0(prop, "%"),
                color = vote),
            size = 18, show.legend = FALSE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  clessnverse::theme_clean_light() +
  ylab("") +
  xlab("\nProjected Vote Share (%)\n") +
  theme(axis.title.x = element_text(size = 26, hjust = 0.5),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21))

ggsave("pres_spsa2024/graphs/pres_spsa24/1_theoretical_example1.png",
       width = 9, height = 6)

# Graph 2 continuous ------------------------------------------------------

data2 <- data %>% 
  tidyr::pivot_longer(., cols = starts_with("continuous"),
               names_to = "party",
               values_to = "pgvs",
               names_prefix = "continuous_") %>% 
  group_by(party, pgvs) %>% 
  summarise(n = n()) %>%
  group_by(party) %>% 
  mutate(prop = n/sum(n)*100,
         vote = ifelse(pgvs %in% c("vote_fragile", "vote_solid"), "Actual Voters", "Other Voters")) %>% 
  group_by(party, vote) %>% 
  mutate(propvote_party = sum(prop),
         party = factor(party, levels = rev(c("A", "B", "C"))),
         fixed = ifelse(pgvs %in% c("vote_solid", "novote_unreach"), 1, 0),
         unreach = ifelse(pgvs == "novote_unreach", 1, 0),
         pgvs = factor(pgvs, levels = rev(c("vote_solid", "vote_fragile", "novote_reachable", "novote_unreach"))),
         pgvs2 = factor(pgvs, levels = c("vote_solid", "vote_fragile", "novote_reachable", "novote_unreach"))) %>% 
  arrange(pgvs2) %>% 
  mutate(cumsum = cumsum(prop),
         start = cumsum - prop,
         mid = ((cumsum - start) / 2) + start,
         labelpgvs = case_when(
           pgvs == "vote_solid" ~ "Solid",
           pgvs == "vote_fragile" ~ "Fragile",
           pgvs == "novote_unreach" ~ "Unreachable",
           pgvs == "novote_reachable" ~ "Reachable"
         ))

ggplot(data2, aes(x = prop, y = party)) +
  geom_col(aes(group = pgvs, fill = factor(unreach)),
           color = NA,
           show.legend = FALSE) +
  scale_fill_manual(values = c("0" = "white", "1" = "grey90")) +
  ggnewscale::new_scale_fill() +
  geom_col(aes(fill = party, alpha = pgvs,
               group = pgvs),
           color = NA,
           show.legend = FALSE) +
  facet_wrap(~factor(vote, levels = c("Actual Voters", "Other Voters"))) +
  geom_text(data = data2 %>% group_by(party) %>%
              filter(vote == "Actual Voters") %>% 
              summarise(prop2 = mean(propvote_party)) %>% 
              mutate(vote = "Actual Voters"),
            aes(x = prop2 + 19,
                label = paste0(prop2, "%"),
                color = party),
            size = 18, show.legend = FALSE) +
  geom_text(aes(label = labelpgvs, x = mid),
            color = "black", angle = 90) +
  geom_text(aes(label = labelpgvs, x = mid,
                color = party), show.legend = FALSE,
            angle = 90, alpha = 0.8) +
  geom_vline(data = data.frame(vote = "Actual Voters",
                               xint = 90),
             aes(xintercept = xint),
             color = "black", linewidth = 3) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_alpha_manual(values = c("vote_solid" = 0.6,
                                "vote_fragile" = 0.3,
                                "novote_reachable" = 0.5,
                                "novote_unreach" = 0)) +
  clessnverse::theme_clean_light() +
  ylab("") +
  xlab("\nProjected Vote Share (%)\n") +
  theme(axis.title.x = element_text(size = 26, hjust = 0.5),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        strip.text.x = element_text(size = 21))

ggsave("pres_spsa2024/graphs/pres_spsa24/1_theoretical_example2.png",
       width = 9, height = 6)



# Scenario 2: C attacks reachable segments --------------------------------

set.seed(123)
data3 <- data.frame(
  id = 1:1000,
  vote = c(rep("A", 240), rep("B", 360), rep("C", 400))
)

data3$continuous_A <- NA
data3$continuous_A[data3$vote == "A"] <- sample(c("vote_solid", "vote_fragile"),
                                              length(data3$continuous_A[data3$vote == "A"]),
                                              prob = c(0.5, 0.5),
                                              replace = TRUE)
data3$continuous_A[data3$vote != "A"] <- sample(c("novote_reachable", "novote_unreach"),
                                              length(data3$continuous_A[data3$vote != "A"]),
                                              prob = c(0.6, 0.4),
                                              replace = TRUE)

data3$continuous_B <- NA
data3$continuous_B[data3$vote == "B"] <- sample(c("vote_solid", "vote_fragile"),
                                              length(data3$continuous_B[data3$vote == "B"]),
                                              prob = c(0.825, 0.175),
                                              replace = TRUE)
data3$continuous_B[data3$vote != "B"] <- sample(c("novote_reachable", "novote_unreach"),
                                              length(data3$continuous_B[data3$vote != "B"]),
                                              prob = c(0.2, 0.8),
                                              replace = TRUE)

data3$continuous_C <- NA
data3$continuous_C[data3$vote == "C"] <- sample(c("vote_solid", "vote_fragile"),
                                              length(data3$continuous_C[data3$vote == "C"]),
                                              prob = c(0.45, 0.55),
                                              replace = TRUE)
data3$continuous_C[data3$vote != "C"] <- sample(c("novote_reachable", "novote_unreach"),
                                              length(data3$continuous_B[data3$vote != "C"]),
                                              prob = c(0.7, 0.3),
                                              replace = TRUE)

data4 <- data3 %>% 
  tidyr::pivot_longer(., cols = starts_with("continuous"),
                      names_to = "party",
                      values_to = "pgvs",
                      names_prefix = "continuous_") %>% 
  group_by(party, pgvs) %>% 
  summarise(n = n()) %>%
  group_by(party) %>% 
  mutate(prop = n/sum(n)*100,
         vote = ifelse(pgvs %in% c("vote_fragile", "vote_solid"), "Actual Voters", "Other Voters")) %>% 
  group_by(party, vote) %>% 
  mutate(propvote_party = sum(prop),
         party = factor(party, levels = (c("A", "B", "C"))),
         fixed = ifelse(pgvs %in% c("vote_solid", "novote_unreach"), 1, 0),
         unreach = ifelse(pgvs == "novote_unreach", 1, 0),
         pgvs = factor(pgvs, levels = rev(c("vote_solid", "vote_fragile", "novote_reachable", "novote_unreach"))),
         pgvs2 = factor(pgvs, levels = c("vote_solid", "vote_fragile", "novote_reachable", "novote_unreach"))) %>% 
  arrange(pgvs2) %>% 
  mutate(cumsum = cumsum(prop),
         start = cumsum - prop,
         mid = ((cumsum - start) / 2) + start,
         labelpgvs = case_when(
           pgvs == "vote_solid" ~ "Solid",
           pgvs == "vote_fragile" ~ "Fragile",
           pgvs == "novote_unreach" ~ "Unreachable",
           pgvs == "novote_reachable" ~ "Reachable"
         ))

ggplot(data4, aes(x = prop, y = party)) +
  geom_col(aes(group = pgvs, fill = factor(unreach)),
           color = NA,
           show.legend = FALSE) +
  scale_fill_manual(values = c("0" = "white", "1" = "grey90")) +
  ggnewscale::new_scale_fill() +
  geom_col(aes(fill = party, alpha = pgvs,
               group = pgvs),
           color = NA,
           show.legend = FALSE) +
  facet_wrap(~factor(vote, levels = c("Actual Voters", "Other Voters"))) +
  geom_text(data = data4 %>% group_by(party) %>%
              filter(vote == "Actual Voters") %>% 
              summarise(prop2 = mean(propvote_party)) %>% 
              mutate(vote = "Actual Voters"),
            aes(x = prop2 + 19,
                label = paste0(prop2, "%"),
                color = party),
            size = 18, show.legend = FALSE) +
  geom_text(aes(label = labelpgvs, x = mid),
            color = "black", angle = 90) +
  geom_text(aes(label = labelpgvs, x = mid,
                color = party), show.legend = FALSE,
            angle = 90, alpha = 0.8) +
  geom_vline(data = data.frame(vote = "Actual Voters",
                               xint = 90),
             aes(xintercept = xint),
             color = "black", linewidth = 3) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_alpha_manual(values = c("vote_solid" = 0.6,
                                "vote_fragile" = 0.3,
                                "novote_reachable" = 0.5,
                                "novote_unreach" = 0)) +
  clessnverse::theme_clean_light() +
  ylab("") +
  xlab("\nProjected Vote Share (%)\n") +
  theme(axis.title.x = element_text(size = 26, hjust = 0.5),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        strip.text.x = element_text(size = 21))

ggsave("pres_spsa2024/graphs/pres_spsa24/1_theoretical_example3.png",
       width = 9, height = 6)



