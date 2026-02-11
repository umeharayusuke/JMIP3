output_dir <- file.path("..", "output/laboseminar")

thema <- "Gro_Emi_CO2"
thema <- "Emi_CO2"
thema <- "Pop"
thema <- "GDP_MER"
thema <- "Gro_Rem_CO2"
thema <- "Prc_Car"
thema <- "Pol_Cos_Cns_Los_rat"
thema <- "Pol_Cos_GDP_Los_rat"
thema <- "Ele_rat_Ele"


CLP <- c("SSP2i_BaU_NoCC_No", "SSP2i_CM15_NoCC_No", "SSP2i_CM7_NoCC_No","SSP2i_CM24_NoCC_No")
CLP <- c("SSP2i_CM13_NoCC_No", "SSP2i_CM15_NoCC_No","SSP2i_CM18_NoCC_No")

df <- rgdx.param("JPN_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema) %>%
  #filter(YEMF == "2100") %>% 
  filter(SCENARIO %in% CLP)

df$SCENARIO <- gsub("SSP2i_BaU_NoCC_No", "BaU", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM7_NoCC_No", "High", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM15_NoCC_No", "Middle", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM24_NoCC_No", "Low", df$SCENARIO)
df$SCENARIO <- factor(df$SCENARIO, levels = c("BaU","High", "Middle", "Low"))

g <- df %>% 
  ggplot(aes(x = YEMF, y = IAMC_Template, group = SCENARIO, color = SCENARIO)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +  
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))+
  #ylab("GDP|MER (billion US$2010/yr)")+
  #ylab("Population (million)")+
  #ylab("Carbon Price (US$2010/yr)")+
  #ylab("Emissions|CO2 (Mt/yr)")+
  #ylab("Policy Cost|Consumption (%)")+
  ylab("Electrification rate (%)")+
  theme_1
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