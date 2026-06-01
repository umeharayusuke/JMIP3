#Cumlative CCS-----------------------------------------------------------
output_dir <- file.path("../..", "output/LaboSeminar")

thema <- "Car_Seq_CCS"
CLP <- paste0("SSP2i_CM", c(1:24), "_NoCC_No")

df <- rgdx.param("JPN_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema) %>%
  filter(SCENARIO %in% CLP) %>%
  mutate(
    YEMF_num = as.numeric(as.character(YEMF))
  ) %>%
  group_by(SCENARIO, YEMF_num) %>%
  summarise(
    IAMC_Template = sum(IAMC_Template, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(SCENARIO, YEMF_num) %>%
  group_by(SCENARIO) %>%
  reframe({
    x0 <- YEMF_num
    y0 <- IAMC_Template

    keep <- !is.na(x0) & !is.na(y0)

    x0 <- x0[keep]
    y0 <- y0[keep]

    years_interp <- seq(min(x0), max(x0), by = 1)

    tibble(
      YEMF_num = years_interp,
      IAMC_Template_interp = approx(
        x = x0,
        y = y0,
        xout = years_interp,
        method = "linear",
        rule = 2
      )$y
    )
  }) %>%
  group_by(SCENARIO) %>%
  arrange(YEMF_num, .by_group = TRUE) %>%
  mutate(
    IAMC_Template_cum = cumsum(IAMC_Template_interp)
  ) %>%
  ungroup()

df$SCENARIO <- factor(
  df$SCENARIO,
  levels = paste0("SSP2i_CM", 1:24, "_NoCC_No")
)

g <- df %>% 
  ggplot(aes(
    x = YEMF_num,
    y = IAMC_Template_cum/1000,
    group = SCENARIO,
    color = SCENARIO
  )) +

  # 3-40
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = 3, ymax = 40,
    fill = "skyblue",
    alpha = 0.15
  ) +

  # 40以上
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = 40, ymax = Inf,
    fill = "purple",
    alpha = 0.12
  ) +

  geom_line(linewidth = 1) +

  scale_color_viridis_d(option = "turbo") +

  scale_x_continuous(
    breaks = c(2020, 2040, 2060, 2080, 2100)
  ) +

  ylab("Cumulative Carbon Sequestration (Gt)") +
  xlab(NULL) +

  theme_1 +
  theme(
    legend.position = "bottom"
  )

plot(g)

name <- paste0(thema, "_cumulative_interp.png")

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 16,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)