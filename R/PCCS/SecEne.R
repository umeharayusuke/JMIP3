output_dir <- file.path("..", "output/CCS")


#CLP <- c("SSP2i_BaU_NoCC_No", "SSP2i_CM13_NoCC_No", "SSP2i_CM15_NoCC_No","SSP2i_CM18_NoCC_No")
vec <- c("Prm_Ene_Coa_w_CCS", 
         "Prm_Ene_Coa_wo_CCS",
         "Prm_Ene_Gas_w_CCS", 
         "Prm_Ene_Gas_wo_CCS", 
         "Prm_Ene_Oil_w_CCS", 
         "Prm_Ene_Oil_wo_CCS",
         "Prm_Ene_Hyd",
         "Prm_Ene_Solar", 
         "Prm_Ene_Win",
         "Prm_Ene_Nuc", 
         "Prm_Ene_Bio_w_CCS",
         "Prm_Ene_Bio_wo_CCS")

col <- c("Coal|w/o CCS" = "grey50", "Coal|w/ CCS" = "grey30", "Oil|w/o CCS" = "tan3",
         "Oil|w/ CCS" = "sandybrown", "Gas|w/o CCS" = "lightgoldenrod", "Gas|w/ CCS" = "lightgoldenrod3",
         "Hydro" = "lightsteelblue", "Nuclear" = "moccasin", "Solar" = "lightsalmon", "Wind" = "lightskyblue3",
         "Biomass|w/o CCS" = "darkolivegreen2", "Biomass|w/ CCS" = "darkolivegreen4", "Geothermal" = "peru")

vec <- gsub("Prm_Ene", "Sec_Ene_Ele", vec)
ylabel <- "Power generation (EJ/yr)"
name <- "Sec.png"

df <- rgdx.param("JPN_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% vec) %>%
  #filter(YEMF == "2100") %>% 
  filter(YEMF %in% c("2050","2100")) %>% 
  filter(SCENARIO %in% CLP)
#df$SCENARIO <- factor(df$SCENARIO, levels = CLP)

df$VEMF <- gsub("Sec_Ene_Ele_Hyd", "Hydro", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Solar", "Solar", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Win", "Wind", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Nuc", "Nuclear", df$VEMF)
#df$VEMF <- gsub("Sec_Ene_Ele_Gas_w_CCS|Sec_Ene_Ele_Gas_wo_CCS|Sec_Ene_Ele_Oil_w_CCS|Sec_Ene_Ele_Oil_wo_CCS|Sec_Ene_Ele_Coa_w_CCS|Sec_Ene_Ele_Coa_wo_CCS", "Fossil Fuels", df$VEMF)
#df$VEMF <- gsub("Sec_Ene_Ele_Bio_w_CCS|Sec_Ene_Ele_Bio_wo_CCS", "Biomass", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Bio_w_CCS", "Biomass|w/ CCS", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Bio_wo_CCS", "Biomass|w/o CCS", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Gas_w_CCS", "Gas|w/ CCS", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Gas_wo_CCS", "Gas|w/o CCS", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Oil_w_CCS", "Oil|w/ CCS", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Oil_wo_CCS", "Oil|w/o CCS", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Coa_w_CCS", "Coal|w/ CCS", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Coa_wo_CCS", "Coal|w/o CCS", df$VEMF)

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



name="SecEne.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)