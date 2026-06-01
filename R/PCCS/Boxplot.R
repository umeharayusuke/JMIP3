
#box plot-------------------------------------------------------
library(tidyverse)
library(gdxrrw)

target_thema <- "Sha_NonBioRen_Ene_Prm_Ene"
target_thema <- "Prc_Car"


CLP <- paste0("SSP2i_CM", 1:24, "_NoCC_No")

df_before <- rgdx.param("JPN_IAMC_Before.gdx", "IAMC_template") %>%
  filter(
    VEMF == target_thema,
    SCENARIO %in% CLP
  ) %>%
  mutate(Case = "Before")

df_after <- rgdx.param("JPN_IAMC_After.gdx", "IAMC_template") %>%
  filter(
    VEMF == target_thema,
    SCENARIO %in% CLP
  ) %>%
  mutate(Case = "After")

df_plot <- bind_rows(df_before, df_after) %>%
  mutate(
    year = as.numeric(as.character(YEMF)),
    Case = factor(Case, levels = c("Before", "After"))
  ) %>%
  filter(!is.na(year))

g <- ggplot(
  df_plot,
  aes(
    x = factor(year),
    y = IAMC_Template,
    fill = Case
  )
) +
  geom_boxplot(
    width = 0.65,
    outlier.shape = 21,
    outlier.size = 1.8,
    outlier.alpha = 0.75,
    position = position_dodge(width = 0.75),
    colour = "#111111",
    linewidth = 0.45
  ) +
  scale_fill_manual(
    values = c(
      "Before" = "#BDBDBD",
      "After"  = "#2C5F8A"
    ),
    name = NULL
  ) +
  labs(
    x = NULL,
    y = "Renewable share (%)",
  #  title = "Renewable Share in Primary Energy",
    title = "Renewable Share in Primary Energy"
  ) +
  theme_plot() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

plot(g)

output_dir <- file.path("../..", "output/LaboSeminar")

ggsave(
  filename = file.path(output_dir, "Sha_Ren_box.png"),
  plot = g,
  width = 12,
  height = 6.5,
  dpi = 300,
  bg = "white"
)
