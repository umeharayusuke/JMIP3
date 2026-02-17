output_dir <- file.path("..", "output/laboseminar")


CLP <- c("SSP2i_BaU_NoCC_No", "SSP2i_CM13_NoCC_No", "SSP2i_CM15_NoCC_No","SSP2i_CM18_NoCC_No")

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
         "CDR(NonAff and NonBECCS)" = "#377EB8")

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
df$VEMF <- gsub("Emi_CO2_Cap_and_Rem", "CDR(NonAff and NonBECCS)", df$VEMF)
df$SCENARIO <- gsub("SSP2i_BaU_NoCC_No", "BaU", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM13_NoCC_No", "High", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM15_NoCC_No", "Middle", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM18_NoCC_No", "Low", df$SCENARIO)
df$SCENARIO <- factor(df$SCENARIO, levels = c("BaU","High", "Middle", "Low"))


g <- ggplot(data = df) +
  geom_bar(mapping = aes(x = SCENARIO, y = IAMC_Template, fill = VEMF), 
           stat = "identity", width = 0.7) +
  scale_fill_manual(values = col)+
  ylab(ylabel)+
  facet_wrap(~ YEMF)+
  theme_1+
  theme(legend.title = element_blank())

plot(g)


df_rate <- df %>%
  select(SCENARIO, VEMF, YEMF, IAMC_Template) %>%
  pivot_wider(names_from = YEMF, values_from = IAMC_Template) %>%
  mutate(diff = `2100` - `2050`)
g_rate <- ggplot(df_rate) +
  geom_col(aes(x = SCENARIO, y = diff, fill = VEMF),
           position = "stack", width = 0.7) +
  scale_fill_manual(values = col) +
  ylab("Difference from 2050 to 2100 (Mt)") +
  theme_1 +
  theme(legend.title = element_blank())


g2 <-(g + g_rate) +
  plot_layout(widths = c(2, 1))+
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.box = "horizontal")

name="Emi_diff.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g2,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)