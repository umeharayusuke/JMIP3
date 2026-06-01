output_dir <- file.path("..", "output/CCS")

thema <- "Gro_Emi_CO2"
thema <- "Emi_CO2"
thema <- "Pop"
thema <- "GDP_MER"
thema <- "Gro_Rem_CO2"
thema <- "Pol_Cos_Cns_Los_rat"
thema <- "Pol_Cos_GDP_Los_rat"
thema <- "Prc_Car"


#CLP <- c("SSP2i_BaU_NoCC_No", "SSP2i_CM15_NoCC_No", "SSP2i_CM7_NoCC_No","SSP2i_CM24_NoCC_No")
#CLP <- c("SSP2i_CM13_NoCC_No", "SSP2i_CM15_NoCC_No","SSP2i_CM18_NoCC_No")

df <- rgdx.param("JPN_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema) %>%
  #filter(YEMF == "2100") %>% 
  filter(SCENARIO %in% CLP)

df$SCENARIO <- factor(
  df$SCENARIO,
  levels = c(
    "SSP2i_BaU_NoCC_No",
    paste0("SSP2i_CM", 1:24, "_NoCC_No")
  )
)

g <- df %>% 
  ggplot(aes(x = YEMF, y = IAMC_Template, group = SCENARIO, color = SCENARIO)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +  
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))+
  #ylab("GDP|MER (billion US$2010/yr)")+
  #ylab("Population (million)")+
  ylab("Carbon Price (US$2010/yr)")+
  ylab("Emissions|CO2 (Mt/yr)")+
  #ylab("Policy Cost|Consumption (%)")+
  #ylab("Policy Cost|GDP (%)")+
  #ylab("Electrification rate (%)")+
  theme_1+
  theme(legend.position = "bottom")

plot(g)

name  <- paste0(thema, ".png")
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

#Emission or emission trade --------------------------------------------------------------------------
thema <- "Gro_Emi_CO2"
thema <- "Pop"
thema <- "GDP_MER"
thema <- "Gro_Rem_CO2"
thema <- "Pol_Cos_Cns_Los_rat"
thema <- "Pol_Cos_GDP_Los_rat"
thema <- "Prc_Car"
thema <- "Emi_CO2"



output_dir <- file.path("../..", "output/LaboSeminar")
CLP <- paste0("SSP2i_CM", c(1:24), "_NoCC_No")

df <- rgdx.param("JPN_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema) %>%
  #filter(YEMF == "2100") %>% 
  filter(SCENARIO %in% CLP)

df$SCENARIO <- factor(
  df$SCENARIO,
  levels = c(
    "SSP2i_BaU_NoCC_No",
    paste0("SSP2i_CM", 1:24, "_NoCC_No")
  )
)

g <- df %>% 
  ggplot(aes(x = YEMF, y = IAMC_Template,group = SCENARIO,color = SCENARIO)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d(option = "turbo") +
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100")) +
  ylab("Emissions|CO2 (Mt/yr)") +
  theme_1 +
  theme(legend.position = "bottom")

  #geom_point(size = 2) +  
  #ylab("GDP|MER (billion US$2010/yr)")+
  #ylab("Population (million)")+
  #ylab("Carbon Price (US$2010/yr)")+
  #ylab("Policy Cost|Consumption (%)")+
  #ylab("Policy Cost|GDP (%)")+
  #ylab("Electrification rate (%)")+

plot(g)

name  <- paste0(thema, ".png")
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 16,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)

