output_dir <- file.path("..", "output/CCS")



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



name="Rem.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)