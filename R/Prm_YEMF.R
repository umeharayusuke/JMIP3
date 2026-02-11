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
name <- "Prm.png"
yvec<-c("2020","2050","2100")

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
df$SCENARIO <- gsub("SSP2i_BaU_NoCC_No", "BaU", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM13_NoCC_No", "NZ+Positive", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM15_NoCC_No", "NZ", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM18_NoCC_No", "NZ+Negative", df$SCENARIO)
df$SCENARIO <- factor(df$SCENARIO, levels = c("NZ+Positive", "NZ", "NZ+Negative"))


g <- ggplot(data = df) +
  geom_bar(mapping = aes(x = YEMF, y = IAMC_Template, fill = VEMF), 
           stat = "identity", width = 0.9) +
  scale_fill_manual(values = col)+
  ylab(ylabel)+
  facet_wrap(~ SCENARIO)+
  theme_1+
  theme(legend.title = element_blank())

plot(g)