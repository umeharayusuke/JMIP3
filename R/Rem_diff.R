output_dir <- file.path("..", "output/laboseminar")


CLP <- c("SSP2i_BaU_NoCC_No", "SSP2i_CM15_NoCC_No", "SSP2i_CM7_NoCC_No","SSP2i_CM24_NoCC_No")

vec <- c("Car_Rem_Bio",
         "Car_Rem_Bio_wit_CCS",
         "Car_Rem_Dir_Air_Cap_wit_CCS",
         "Car_Rem_Enh_Wea", 
         "Car_Rem_Frs",
         "Car_Rem_Soi_Car_Seq")

col <- c(
  "BECCS" = "#4DAF4A",          
  "Biochar" = "#E69F00",       
  "Soil Carbon" = "#A65628",    
  "Afforestation" = "#1B7837",  
  "Enhanced Weather" = "#377EB8",
  "DACCS" = "#984EA3"           
)
ylabel <- "Carbon Removal (Mt)"
name <- "Rem.png"


df <- rgdx.param("JPN_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% vec) %>%
  #filter(YEMF == "2100") %>% 
  filter(YEMF %in% c("2050","2100")) %>% 
  filter(SCENARIO %in% CLP)
#df$SCENARIO <- factor(df$SCENARIO, levels = CLP)

# Emi ---------------------------------------------------------------------
df$VEMF <- gsub("Car_Rem_Bio_wit_CCS", "BECCS", df$VEMF)
df$VEMF <- gsub("Car_Rem_Bio", "Biochar", df$VEMF)
df$VEMF <- gsub("Car_Rem_Dir_Air_Cap_wit_CCS", "DACCS", df$VEMF)
df$VEMF <- gsub("Car_Rem_Enh_Wea", "Enhanced Weather", df$VEMF)
df$VEMF <- gsub("Car_Rem_Frs", "Afforestation", df$VEMF)
df$VEMF <- gsub("Car_Rem_Soi_Car_Seq", "Soil Carbon", df$VEMF)
df$SCENARIO <- gsub("SSP2i_BaU_NoCC_No", "BaU", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM7_NoCC_No", "High", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM15_NoCC_No", "Middle", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM24_NoCC_No", "Low", df$SCENARIO)
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

name="Rem_diff.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g2,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)