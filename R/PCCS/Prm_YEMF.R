CLP <- c("SSP2i_CM18_NoCC_No")
CLP <- c("SSP2i_CM13_NoCC_No", "SSP2i_CM15_NoCC_No","SSP2i_CM18_NoCC_No")


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
ylabel <- "Primary energy (EJ/yr)"


df <- rgdx.param("JPN_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% vec) %>%
  #filter(YEMF == "2100") %>% 
  #filter(YEMF %in% yvec) %>% 
  filter(SCENARIO %in% CLP)
#df$SCENARIO <- factor(df$SCENARIO, levels = CLP)

df$VEMF <- gsub("Prm_Ene_Hyd", "Hydro", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Solar", "Solar", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Win", "Wind", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Nuc", "Nuclear", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Bio_w_CCS", "Biomass|w/ CCS", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Bio_wo_CCS", "Biomass|w/o CCS", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Gas_w_CCS", "Gas|w/ CCS", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Gas_wo_CCS", "Gas|w/o CCS", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Oil_w_CCS", "Oil|w/ CCS", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Oil_wo_CCS", "Oil|w/o CCS", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Coa_w_CCS", "Coal|w/ CCS", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Coa_wo_CCS", "Coal|w/o CCS", df$VEMF)

df$SCENARIO <- factor(
  df$SCENARIO,
  levels = c(
    paste0("SSP2i_CM", 1:24, "_NoCC_No")
  )
)
df <- df %>%
  mutate(
    CM_num = as.numeric(gsub(".*CM(\\d+)_.*", "\\1", SCENARIO)),
    group = case_when(
      CM_num <= 6  ~ "CM1-6",
      CM_num <= 12 ~ "CM7-12",
      CM_num <= 18 ~ "CM13-18",
      TRUE         ~ "CM19-24"
    )
  )

df$SCENARIO <- factor(df$SCENARIO,
                      levels = paste0("SSP2i_CM", 1:24, "_NoCC_No"))

df$group <- factor(df$group,
                   levels = c("CM1-6", "CM7-12", "CM13-18", "CM19-24"))

g <- ggplot(data = df) +
  geom_bar(aes(x = YEMF, y = IAMC_Template, fill = VEMF), 
           stat = "identity", width = 0.9) +
  
  scale_fill_manual(values = col) +
  ylab(ylabel) +
  
  facet_wrap(~ SCENARIO, ncol = 6)

plot(g)


name="Prm_YEMF.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)