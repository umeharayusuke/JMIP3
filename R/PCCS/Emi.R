output_dir <- file.path("..", "output/CCS")



vec<- c("Emi_CO2_AFO",
        "Emi_CO2_Ene_Sup",
        "Emi_CO2_Ene_Dem",
        "Emi_CO2_Ind_Pro",
        "Emi_CO2_Pro_Use",
        "Emi_CO2_Cap_and_Rem")

col <- c("AFOLU" = "#FC8D62",
         "Energy Supply" = "#66C2A5",
         "Energy Demand" = "#8DA0CB",
         "Industrial Processes" = "#984EA3",
         "Product Use" = "#FFFF33",
         "CDR" = "#377EB8")

ylabel <- "CO2 emission (Mt)"
name <- "Emi.png"

df <- rgdx.param("JPN_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% vec) %>%
  #filter(YEMF == "2100") %>% 
  filter(YEMF %in% c("2050","2100")) %>% 
  filter(SCENARIO %in% CLP)
#df$SCENARIO <- factor(df$SCENARIO, levels = CLP)

# Emi ---------------------------------------------------------------------
df$VEMF <- gsub("Emi_CO2_AFO", "AFOLU", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Ene_Sup", "Energy Supply", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Ene_Dem", "Energy Demand", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Ind_Pro", "Industrial Processes", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Pro_Use", "Product Use", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Cap_and_Rem", "CDR", df$VEMF)

df$SCENARIO <- factor(
  df$SCENARIO,
  levels = c(
    "SSP2i_BaU_NoCC_No",
    paste0("SSP2i_CM", 1:24, "_NoCC_No")
  )
)


g <- ggplot(data = df) +
  geom_bar(mapping = aes(x = SCENARIO, y = IAMC_Template, fill = VEMF), 
           stat = "identity", width = 0.7) +
  scale_fill_manual(values = col)+
  ylab(ylabel)+
  facet_wrap(~ YEMF)+
  theme_1+
  theme(legend.title = element_blank())

plot(g)


name="Emi.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)