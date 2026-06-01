library(tidyverse)

df <- tribble(
  ~year, ~value, ~group, ~category,
  2024, 0.3, "Current", "Current",
  2050, 132, "JMIP2",   "Model studies",
  2050, 150, "TIMES",   "Model studies",
  2050, 250, "AIM",     "Model studies"
)
line_df <- tribble(
  ~year, ~value,
  2024, 0.3,
  2030, 9,
  2050, 180
)
target_range <- tribble(
  ~year, ~ymin, ~ymax, ~category,
  2030, 6,   12,  "Target",
  2050, 120, 240, "Target"
)
p <- ggplot() +
  geom_linerange(
    data = target_range,
    aes(
      x = year,
      ymin = ymin,
      ymax = ymax,
      colour = category
    ),
    linewidth = 3.8,
    alpha = 0.95
  ) +
  geom_line(
    data = line_df,
    aes(
      x = year,
      y = value
    ),
    linewidth = 1.2,
    colour = "#8C8C8C"
  ) +
  geom_point(
    data = df,
    aes(
      x = year,
      y = value,
      colour = category,
      shape = category
    ),
    size = 5.8,
    alpha = 0.98
  ) +
geom_text(
  data = df %>% filter(year == 2024),
  aes(
    x = year,
    y = value,
    label = "0.3"
  ),
  nudge_y = 15,
  size = 5.3,
  family = "Times New Roman",
  fontface = "bold",
  colour = "#111111"
) +

geom_text(
  data = df %>% filter(year == 2050),
  aes(
    x = year,
    y = value,
    label = round(value)
  ),
  hjust = -0.3,
  vjust = 0.5,
  size = 5.3,
  family = "Times New Roman",
  fontface = "bold",
  colour = "#111111"
) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "Current" = "#111111",
      "Model studies" = "#C9252D",
      "Target" = "#2C5F8A"
    )
  ) +

  scale_shape_manual(
    name = NULL,
    values = c(
      "Current" = 16,
      "Model studies" = 17,
      "Target" = 15
    )
  ) +

  scale_x_continuous(
    breaks = c(2024, 2030, 2050),
    limits = c(2022, 2056),
    expand = expansion(mult = c(0.01, 0.02))
  ) +

  scale_y_continuous(
    breaks = c(0, 100, 200),
    limits = c(0, 260),
    expand = expansion(mult = c(0, 0.04))
  ) +

  labs(
    x = NULL,
    y = expression(MtCO[2]~yr^{-1}),
    title = "CCS Deployment Pathways in Japan"
  ) +

  guides(
    colour = guide_legend(
      title = NULL,
      order = 1,
      override.aes = list(
        size = c(6, 6, 6),
        linewidth = c(0, 0, 3.8),
        alpha = 1
      )
    ),
    shape = "none"
  ) +

  theme_plot()

p

output_dir <- file.path("output/LaboSeminar")
name="CCS_Japan_target.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = p,
  width = 10,
  height = 6,
  dpi = 600,
  bg = "white"
)
