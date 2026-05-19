output_dir <- file.path("..", "output/CCS")


vec <- c("Fin_Ene_Ele",
         "Fin_Ene_Gas",
         "Fin_Ene_Heat",
         "Fin_Ene_Hyd",
         "Fin_Ene_Liq_Oil",
         "Fin_Ene_Liq_Bio",
         "Fin_Ene_SolidsCoa",
         "Fin_Ene_SolidsBio")

col <- c( 
  "Coal"="grey70",
  "Oil"="sandybrown",
  "Gas"="moccasin",
  "Biomass"="#A9D65D",
  "Biofuel"="#DBFF70",
  "Electricity"="lightsteelblue",
  "Heat"="salmon",
  "Hydrogen"="thistle2")
ylabel <- "Final energy (EJ/yr)"

df <- rgdx.param("JPN_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% vec) %>%
  #filter(YEMF == "2100") %>% 
  filter(YEMF %in% c("2050","2100")) %>% 
  filter(SCENARIO %in% CLP)
#df$SCENARIO <- factor(df$SCENARIO, levels = CLP)

df$VEMF <- gsub("Fin_Ene_Ele", "Electricity", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Gas", "Gas", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Heat", "Heat", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Hyd", "Hydrogen", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Liq_Oil", "Oil", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Liq_Bio", "Biofuel", df$VEMF)
df$VEMF <- gsub("Fin_Ene_SolidsCoa", "Coal", df$VEMF)
df$VEMF <- gsub("Fin_Ene_SolidsBio", "Biomass", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Ind", "Industry", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Res_and_Com", "Buildings", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Tra", "Transport", df$VEMF)

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

name <- "Fin.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)