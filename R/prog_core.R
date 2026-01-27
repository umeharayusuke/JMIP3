# vec ---------------------------------------------------------------------

df <- rgdx.param("JPN_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% vec) %>%
  filter(YEMF == "2100") %>% 
  filter(SCENARIO %in% CLP)
df$SCENARIO <- factor(df$SCENARIO, levels = CLP)

# gsub --------------------------------------------------------------------
# Prm ---------------------------------------------------------------------
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
# Sec ---------------------------------------------------------------------
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
# Fin ---------------------------------------------------------------------
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
# Rem ---------------------------------------------------------------------
df$VEMF <- gsub("Car_Rem_Bio_wit_CCS", "BECCS", df$VEMF)
df$VEMF <- gsub("Car_Rem_Bio", "Biochar", df$VEMF)
df$VEMF <- gsub("Car_Rem_Dir_Air_Cap_wit_CCS", "DACCS", df$VEMF)
df$VEMF <- gsub("Car_Rem_Enh_Wea", "Enhanced Weather", df$VEMF)
df$VEMF <- gsub("Car_Rem_Frs", "Afforestation", df$VEMF)
df$VEMF <- gsub("Car_Rem_Soi_Car_Seq", "Soil Carbon", df$VEMF)
# Emi ---------------------------------------------------------------------
df$VEMF <- gsub("Emi_CO2_AFO", "AFOLU", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Ene_Sup", "Energy Supply", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Ene_Dem", "Energy Demand", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Ind_Pro", "Industrial Processes", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Pro_Use", "Product Use", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Cap_and_Rem", "CDR(NonAff and NonBECCS)", df$VEMF)

df$SCENARIO <- gsub("SSP2i_BaU_NoCC_No", "BaU", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM1_NoCC_No", "CM7", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM2_NoCC_No", "CM8", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM3_NoCC_No", "CM9", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM4_NoCC_No", "CM10", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM5_NoCC_No", "CM11", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM6_NoCC_No", "CM12", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM7_NoCC_No", "CM1", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM8_NoCC_No", "CM2", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM9_NoCC_No", "CM3", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM10_NoCC_No", "CM4", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM11_NoCC_No", "CM5", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM12_NoCC_No", "CM6", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM13_NoCC_No", "CM13", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM14_NoCC_No", "CM14", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM15_NoCC_No", "CM15", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM16_NoCC_No", "CM16", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM17_NoCC_No", "CM17", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM18_NoCC_No", "CM18", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM19_NoCC_No", "CM19", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM20_NoCC_No", "CM20", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM21_NoCC_No", "CM21", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM22_NoCC_No", "CM22", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM23_NoCC_No", "CM23", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM24_NoCC_No", "CM24", df$SCENARIO)
df$SCENARIO <- factor(df$SCENARIO, levels = c("BaU", paste0("CM", 1:24)))



# plot --------------------------------------------------------------------
g <- ggplot(data = df) +
  geom_bar(mapping = aes(x = SCENARIO, y = IAMC_Template, fill = VEMF), 
           stat = "identity", width = 0.7) +
  scale_fill_manual(values = col)+
  ylab("Carbon removal in 2100 (Mt)")+
  theme_1
plot(g)


name  <- "rem.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

# nonvec ------------------------------------------------------------------

thema <- "Gro_Emi_CO2"
thema <- "Emi_CO2"
thema <- "Pop"
thema <- "GDP_PPP"
thema <- "Gro_Rem_CO2"
thema <- "Prc_Car"



df <- rgdx.param("JPN_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema) %>%
  #filter(YEMF == "2100") %>% 
  filter(SCENARIO %in% CLP)

df$SCENARIO <- gsub("SSP2i_BaU_NoCC_No", "BaU", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM1_NoCC_No", "CM7", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM2_NoCC_No", "CM8", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM3_NoCC_No", "CM9", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM4_NoCC_No", "CM10", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM5_NoCC_No", "CM11", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM6_NoCC_No", "CM12", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM7_NoCC_No", "CM1", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM8_NoCC_No", "CM2", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM9_NoCC_No", "CM3", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM10_NoCC_No", "CM4", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM11_NoCC_No", "CM5", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM12_NoCC_No", "CM6", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM13_NoCC_No", "CM13", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM14_NoCC_No", "CM14", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM15_NoCC_No", "CM15", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM16_NoCC_No", "CM16", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM17_NoCC_No", "CM17", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM18_NoCC_No", "CM18", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM19_NoCC_No", "CM19", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM20_NoCC_No", "CM20", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM21_NoCC_No", "CM21", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM22_NoCC_No", "CM22", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM23_NoCC_No", "CM23", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM24_NoCC_No", "CM24", df$SCENARIO)
df$SCENARIO <- factor(df$SCENARIO, levels = c("BaU", paste0("CM", 1:24)))


g <- df %>% 
  ggplot(aes(x = YEMF, y = IAMC_Template, group = SCENARIO, color = SCENARIO)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +  
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))+
  ylab(paste0(thema,  "(US$)"))+
  theme_1
plot(g)


name  <- paste0(thema, "19-24.png")
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
