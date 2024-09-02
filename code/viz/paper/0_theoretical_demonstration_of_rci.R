# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

## i want a tibble with 3 columns: party, method, value
data <- tibble::tibble(party = rep(c("A", "B", "C"), 3),
                       method = c(rep("PTV", 3), rep("RCI", 3), rep("Traditional\nVote Intention", 3)),
                       value = c(10, 6, -8, 2, -2, -9, 5, -5, -5),
                       label = c(10, 8, 1, 2, -2, -9, 1, 0, 0)
                      ) %>% 
  mutate(
    party = factor(party, levels = rev(c("A", "B", "C"))),
    method = factor(method, levels = c("Traditional\nVote Intention", "PTV", "RCI"))
)

## Graph -------------------------------------------------------------------

colors <- c("A" = "cyan", "B" = "magenta", "C" = "gold")

axis <- data.frame(
  label = c(
    seq(from = -10, to = 10, by = 1),
    "0 (Non-voter)", "1 (Voter)",
    seq(from = 0, to = 10, by = 1)),
  x = c(
    seq(from = -10, to = 10, by = 1),
    -5, 5,
    seq(from = -10, to = 10, by = 2)
  ),
  method = c(rep("RCI", 21), rep("Traditional\nVote Intention", 2), rep("PTV", 11))
) |> 
  mutate(
    method = factor(method, levels = c("Traditional\nVote Intention", "PTV", "RCI"))
  )

vline <- data.frame(
  party = "A",
  x = c(0, 0),
  method = c("RCI", "Traditional\nVote Intention")
) |> 
  mutate(
    method = factor(method, levels = c("Traditional\nVote Intention", "PTV", "RCI"))
  )

ggplot(data, aes(x = value, y = party)) +
  geom_point(aes(color = party),
             show.legend = FALSE,
             size = 4, alpha = 0.75,
             shape = 19, stroke = NA) +
  scale_color_manual(values = colors) +
  ggnewscale::new_scale_color() +
  geom_text(
    aes(color = party, label = label),
        show.legend = FALSE,
        size = 1.5
  ) +
  scale_color_manual(values = colorspace::darken(colors, amount = 0.4)) +
  facet_wrap(~method,
             strip.position = "bottom") +
  clessnize::theme_clean_light(base_size = 5) +
  scale_x_continuous(limits = c(-10, 10)) +
  ylab("Hypotethical Party\n") +
  scale_y_discrete(expand = expansion(mult = c(0.3, 0.2))) +
  geom_linerange(
    data = vline,
    aes(ymin = 0.75, ymax = 3.25),
        x = 0, linewidth = 1.5,
        color = "grey60") +
  geom_text(data = axis, size = 1.5,
            aes(label = label, x = x),
            y = 0.6, color = "grey30") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, color = "grey30"),
        axis.text.y = element_text(size = 16, color = "grey30"),
        panel.background = element_rect(color = "grey90"),
        panel.grid.major.y = element_blank(),
        strip.text.x = element_text(size = 13, color = "grey30"))

ggsave("_SharedFolder_article_pot-growth/graphs/paper/0_theoretical_demonstration_of_rci.png",
       width = 6, height = 3)
