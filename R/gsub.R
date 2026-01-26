
# Prm ---------------------------------------------------------------------



df$VEMF <- gsub("Prm_Ene_Hyd", "Hydro", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Solar", "Solar", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Win", "Wind", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Nuc", "Nuclear", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Coa_w_CCS|Prm_Ene_Coa_wo_CCS|Prm_Ene_Oil_w_CCS|Prm_Ene_Oil_wo_CCS|Prm_Ene_Gas_w_CCS|Prm_Ene_Gas_wo_CCS", 
                 "Fossil Fuels", df$VEMF)
df$VEMF <- gsub("Prm_Ene_Bio_w_CCS|Prm_Ene_Bio_wo_CCS", 
                 "Biomass", df$VEMF)


# Sec ---------------------------------------------------------------------

df$VEMF <- gsub("Sec_Ene_Ele_Hyd", "Hydro", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Solar", "Solar", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Win", "Wind", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Nuc", "Nuclear", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Gas_w_CCS|Sec_Ene_Ele_Gas_wo_CCS|Sec_Ene_Ele_Oil_w_CCS|Sec_Ene_Ele_Oil_wo_CCS|Sec_Ene_Ele_Coa_w_CCS|Sec_Ene_Ele_Coa_wo_CCS", "Fossil Fuels", df$VEMF)
df$VEMF <- gsub("Sec_Ene_Ele_Bio_w_CCS|Sec_Ene_Ele_Bio_wo_CCS", "Biomass", df$VEMF)



# Fin ---------------------------------------------------------------------


df$VEMF <- gsub("Fin_Ene_Ele", "Electricity", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Gas", "Gas", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Heat", "Heat", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Hyd", "Hydrogen", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Liq_Oil", "Oil", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Liq_Bio", "Biofuel", df$VEMF)
df$VEMF <- gsub("Fin_Ene_SolidsCoa", "Coal", df$VEMF)
df$VEMF <- gsub("Fin_Ene_SolidsBio", "Biomass", df$VEMF)


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


