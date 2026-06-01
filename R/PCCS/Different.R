
#Difference between old and new-------------------------------
thema <- "Gro_Emi_CO2"
thema <- "Pop"
thema <- "GDP_MER"
thema <- "Gro_Rem_CO2"
thema <- "Pol_Cos_Cns_Los_rat"
thema <- "Pol_Cos_GDP_Los_rat"
thema <- "Prc_Car"
thema <- "Emi_CO2"
thema <- "Trd_Emi_All_Vol"
thema <- "Pol_Cos_Cns_Los_rat_NPV_5pc"
thema <- "Pol_Cos_GDP_Los_rat_NPV_5pc"


CLP <- paste0("SSP2i_CM", 1:24, "_NoCC_No")

df_before <- rgdx.param("JPN_IAMC_Before.gdx", "IAMC_template") %>%
  filter(
    VEMF == thema,
    YEMF == "2100",
    SCENARIO %in% CLP
  ) %>%
  mutate(Case = "Before")

df_after <- rgdx.param("JPN_IAMC_After.gdx", "IAMC_template") %>%
  filter(
    VEMF == thema,
    YEMF == "2100",
    SCENARIO %in% CLP
  ) %>%
  mutate(Case = "After")

df_compare <- bind_rows(df_before, df_after)

df_compare$SCENARIO <- factor(
  df_compare$SCENARIO,
  levels = paste0("SSP2i_CM", 1:24, "_NoCC_No")
)

df_compare$Case <- factor(df_compare$Case, levels = c("Before", "After"))

g <- df_compare %>%
  ggplot(aes(
    x = IAMC_Template,
    y = SCENARIO,
    color = Case
  )) +
  geom_line(
    aes(group = SCENARIO),
    color = "grey60",
    linewidth = 0.6
  ) +
  geom_point(size = 3) +
  xlab("Carbon Price (US2010$/tCO2)") +
  ylab("Scenario") +
  labs(title="Cumulative GDP loss rate (%)")+
  labs(title="Carbon Price (US2010$/tCO2)")+
  theme_plot() +
  theme(
    axis.title.y = element_text(),
    legend.position = "bottom"
  )

plot(g)

name <- paste0(thema, ".png")

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 16,
  height = 10,
  dpi = 300,
  bg = "white"
)
#Difference between old and new with panel-------------------------------

themas <- c(
  "Pol_Cos_Cns_Los_rat_NPV_5pc",
  "Pol_Cos_GDP_Los_rat_NPV_5pc",
  "Prc_Car",
  "Trd_Emi_All_Val"
)
themas <- c("Ele_rat_Ele", 
         "Sha_NonBioRen_Ene_Prm_Ene",
         "Ene_Its", 
         "Car_Its")

thema_labels <- c(
  "Pol_Cos_Cns_Los_rat_NPV_5pc" = "(a) Cumulative consumption loss rate (%)",
  "Pol_Cos_GDP_Los_rat_NPV_5pc" = "(b) Cumulative GDP loss rate (%)",
  "Prc_Car"                    = "(c) Carbon price (USD2010/tCO2)",
  "Trd_Emi_All_Val"            = "(d) Emissions trading value (Million USD2010)"
)

CLP <- paste0("SSP2i_CM", 1:24, "_NoCC_No")

df_before <- rgdx.param("JPN_IAMC_Before.gdx", "IAMC_template") %>%
  filter(
    VEMF %in% themas,
    YEMF == "2100",
    SCENARIO %in% CLP
  ) %>%
  mutate(Case = "Before")

df_after <- rgdx.param("JPN_IAMC_After.gdx", "IAMC_template") %>%
  filter(
    VEMF %in% themas,
    YEMF == "2100",
    SCENARIO %in% CLP
  ) %>%
  mutate(Case = "After")

df_compare <- bind_rows(df_before, df_after) %>%
  mutate(
    SCENARIO = factor(
      SCENARIO,
      levels = paste0("SSP2i_CM", 1:24, "_NoCC_No")
    ),
    Case = factor(Case, levels = c("Before", "After")),
    VEMF = factor(VEMF, levels = themas, labels = thema_labels)
  )

g <- df_compare %>%
  ggplot(aes(
    x = IAMC_Template,
    y = SCENARIO,
    color = Case
  )) +
  geom_line(
    aes(group = SCENARIO),
    color = "grey60",
    linewidth = 0.6
  ) +
  geom_point(size = 2.5) +
  facet_wrap(~ VEMF, ncol = 2, scales = "free_x") +
  xlab(NULL) +
  ylab("Scenario") +
  theme_1 +
  theme(
    axis.title.y = element_text(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

plot(g)

ggsave(
  filename = file.path(output_dir, "Before_After_Comparison_4panel.png"),
  plot = g,
  width = 16,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)


# Difference between old and new -------------------------------

themas <- c(
  "Ele_rat_Ele",
  "Sha_NonBioRen_Ene_Prm_Ene",
  "Ene_Its",
  "Car_Its"
)

thema_labels <- c(
  "Ele_rat_Ele"                 = "(a) Electrification rate (%)",
  "Sha_NonBioRen_Ene_Prm_Ene"   = "(b) Renewable share (%)",
  "Ene_Its"                     = "(c) Energy intensity (PE GJ/GDP US2010$)",
  "Car_Its"                     = "(d) Carbon intensity (CO2 Mt/PE EJ)"
)

CLP <- paste0("SSP2i_CM", 1:24, "_NoCC_No")

df_before <- rgdx.param("JPN_IAMC_Before.gdx", "IAMC_template") %>%
  filter(
    VEMF %in% themas,
    YEMF == "2100",
    SCENARIO %in% CLP
  ) %>%
  mutate(Case = "Before")

df_after <- rgdx.param("JPN_IAMC_After.gdx", "IAMC_template") %>%
  filter(
    VEMF %in% themas,
    YEMF == "2100",
    SCENARIO %in% CLP
  ) %>%
  mutate(Case = "After")

df_compare <- bind_rows(df_before, df_after) %>%
  mutate(
    SCENARIO = factor(SCENARIO, levels = CLP),
    Case = factor(Case, levels = c("Before", "After")),
    VEMF = factor(VEMF, levels = themas, labels = thema_labels)
  )

g <- df_compare %>%
  ggplot(aes(
    x = IAMC_Template,
    y = SCENARIO,
    color = Case
  )) +
  geom_line(
    aes(group = SCENARIO),
    color = "grey60",
    linewidth = 0.6
  ) +
  geom_point(size = 2.5) +
  facet_wrap(~ VEMF, ncol = 2, scales = "free_x") +
  xlab(NULL) +
  ylab("Scenario") +
  labs(title = "Difference between old and new") +
  theme_plot() +
  theme(
    axis.title.y = element_text(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

plot(g)

ggsave(
  filename = file.path(output_dir, "Before_After_Comparison_energy_4panel.png"),
  plot = g,
  width = 16,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)