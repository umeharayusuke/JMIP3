
# setting -----------------------------------------------------------------



library(tidyverse)
library(dplyr)
library(ggplot2)
library(gdxrrw)
library(stringr)
library(gridExtra)
library(patchwork)
library(cowplot)
library(lemon)
library(purrr)
library(rnaturalearthdata)
library(rnaturalearth)

theme_1 <- theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, size = 16, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = "right", 
        #legend.title = element_blank(),
        strip.background = element_blank())
theme_1 <- theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, size = 16, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = "right")
setwd("data")

output_dir <- file.path("..", "output/biomin")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


# vector ------------------------------------------------------------------

# CLP ---------------------------------------------------------------------


CLP <- c("SSP2i_BaU_NoCC_No",
         "SSP2i_CM1_NoCC_No",
         "SSP2i_CM2_NoCC_No",
         "SSP2i_CM3_NoCC_No",
         "SSP2i_CM4_NoCC_No",
         "SSP2i_CM5_NoCC_No",
         "SSP2i_CM6_NoCC_No",
         "SSP2i_CM7_NoCC_No",
         "SSP2i_CM8_NoCC_No",
         "SSP2i_CM9_NoCC_No",
         "SSP2i_CM10_NoCC_No",
         "SSP2i_CM11_NoCC_No",
         "SSP2i_CM12_NoCC_No",
         "SSP2i_CM13_NoCC_No",
         "SSP2i_CM14_NoCC_No",
         "SSP2i_CM15_NoCC_No",
         "SSP2i_CM16_NoCC_No",
         "SSP2i_CM17_NoCC_No",
         "SSP2i_CM18_NoCC_No",
         "SSP2i_CM19_NoCC_No",
         "SSP2i_CM20_NoCC_No",
         "SSP2i_CM21_NoCC_No",
         "SSP2i_CM22_NoCC_No",
         "SSP2i_CM23_NoCC_No",
         "SSP2i_CM24_NoCC_No")


CLP <- c("SSP2i_CM1_NoCC_No","SSP2i_CM2_NoCC_No","SSP2i_CM3_NoCC_No","SSP2i_CM4_NoCC_No","SSP2i_CM5_NoCC_No","SSP2i_CM6_NoCC_No")
CLP <- c("SSP2i_CM7_NoCC_No","SSP2i_CM8_NoCC_No", "SSP2i_CM9_NoCC_No","SSP2i_CM10_NoCC_No","SSP2i_CM11_NoCC_No","SSP2i_CM12_NoCC_No")
CLP <- c("SSP2i_CM13_NoCC_No","SSP2i_CM14_NoCC_No", "SSP2i_CM15_NoCC_No","SSP2i_CM16_NoCC_No","SSP2i_CM17_NoCC_No","SSP2i_CM18_NoCC_No")
CLP <- c("SSP2i_CM19_NoCC_No","SSP2i_CM20_NoCC_No", "SSP2i_CM21_NoCC_No","SSP2i_CM22_NoCC_No","SSP2i_CM23_NoCC_No","SSP2i_CM24_NoCC_No")
CLP <- c("SSP2i_CM24_NoCC_No")
CLP <- c("SSP2i_BaU_NoCC_No", "SSP2i_CM15_NoCC_No", "SSP2i_CM7_NoCC_No","SSP2i_CM24_NoCC_No")
# Prm_Ene -----------------------------------------------------------------


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

#col = c("Biomass" = "darkolivegreen2", "Fossil Fuels" = "gray60",  "Hydro" = "lightsteelblue", 
#        "Nuclear" = "moccasin",  "Solar" = "lightsalmon", "Wind" = "lightskyblue3")

col <- c("Coal|w/o CCS" = "grey50", "Coal|w/ CCS" = "grey30", "Oil|w/o CCS" = "tan3",
         "Oil|w/ CCS" = "sandybrown", "Gas|w/o CCS" = "lightgoldenrod", "Gas|w/ CCS" = "lightgoldenrod3",
         "Hydro" = "lightsteelblue", "Nuclear" = "moccasin", "Solar" = "lightsalmon", "Wind" = "lightskyblue3",
         "Biomass|w/o CCS" = "darkolivegreen2", "Biomass|w/ CCS" = "darkolivegreen4", "Geothermal" = "peru")

ylabel <- "Primary energy (EJ/yr)"
name <- "Prm.png"
# Sec_Ene -----------------------------------------------------------------


vec <- gsub("Prm_Ene", "Sec_Ene_Ele", vec)

ylabel <- "Power generation (EJ/yr)"
name <- "Sec.png"
# Fin_Ene -----------------------------------------------------------------


vec <- c("Fin_Ene_Ele",
         "Fin_Ene_Gas",
         "Fin_Ene_Heat",
         "Fin_Ene_Hyd",
         "Fin_Ene_Liq_Oil",
         "Fin_Ene_Liq_Bio",
         "Fin_Ene_SolidsCoa",
         "Fin_Ene_SolidsBio")

#vec <- c("Fin_Ene_Ind","Fin_Ene_Res_and_Com","Fin_Ene_Tra")

col <- c( 
  "Coal"="grey70",
  "Oil"="sandybrown",
  "Gas"="moccasin",
  "Biomass"="#A9D65D",
  "Biofuel"="#DBFF70",
  "Electricity"="lightsteelblue",
  "Heat"="salmon",
  "Hydrogen"="thistle2"
)

ylabel <- "Final energy (EJ/yr)"
name <- "Fin.png"
# Emi ---------------------------------------------------------------------


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
# Rem ---------------------------------------------------------------------


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

