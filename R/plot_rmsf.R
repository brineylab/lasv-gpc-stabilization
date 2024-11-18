library(tidyverse)

subsample_colors <-
  c(
    "64" = "",
    "128" = "",
    "192" = "",
    "256" = ""
  )

uncleaved_rmsf <- read_csv("data/uncleaved_subsample_rmsf.csv") |>
  janitor::clean_names() |>
  pivot_longer(
    -residue_number,
    names_to = "name",
    values_to = "rmsf"
  ) |>
  mutate(
    subsample = factor(parse_number(name)),
    residue_number = residue_number + 58
  ) |>
  select(subsample, residue_number, rmsf)

uncleaved_rmsf |>
  ggplot(
    aes(
      residue_number,
      rmsf,
      linetype = subsample
    )
  ) +
  annotate(
    geom = "rect",
    ymin = 0, ymax = 30, xmin = 59, xmax = 259,
    fill = "#e8eef1",
    alpha = 0.3,
  ) +
  # NFL
  annotate(
    geom = "rect",
    ymin = 0, ymax = 30, xmin = 260, xmax = 275,
    fill = "#ff595e",
    alpha = 0.3,
  ) +
  # IFL
  annotate(
    geom = "rect",
    ymin = 0, ymax = 30, xmin = 276, xmax = 295,
    fill = "#f37324",
    alpha = 0.3,
  ) +
  # HR1
  annotate(
    geom = "rect",
    ymin = 0, ymax = 30, xmin = 306, xmax = 359,
    fill = "#ffca3a",
    alpha = 0.3,
  ) +
  # T-Loop
  annotate(
    geom = "rect",
    ymin = 0, ymax = 30, xmin = 364, xmax = 385,
    fill = "#8ac926",
    alpha = 0.3,
  ) +
  # HR2
  annotate(
    geom = "rect",
    ymin = 0, ymax = 30, xmin = 386, xmax = 416,
    fill = "#1982c4",
    alpha = 0.3,
  ) +
  # MPER
  annotate(
    geom = "rect",
    ymin = 0, ymax = 30, xmin = 417, xmax = 427,
    fill = "#6a4c93",
    alpha = 0.3,
  ) +
  geom_line(
    linewidth = 0.25,
    color = "gray40"
  ) +
  scale_linetype_manual(values = c(3, 4, 2, 1)) +
  labs(
    x = "Residue Number",
    y = "RMSF",
    linetype = NULL,
    title = "Uncleaved"
  ) +
  theme(
    legend.position = "bottom",
    panel.border = element_blank(),
    axis.ticks = element_line(linewidth = 0.15, color = "gray20"),
    axis.ticks.length = unit(0.2, "cm")
  )

ggsave(
  "img/uncleaved_subsample_rmsf.png",
  width = 7.5,
  height = 3
)
