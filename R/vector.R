
# CLP ---------------------------------------------------------------------


CLP <- c("SSP2i_BaU_NoCC_No",
         "SSP2i_CM7_NoCC_No",
         "SSP2i_CM8_NoCC_No",
         "SSP2i_CM9_NoCC_No",
         "SSP2i_CM10_NoCC_No",
         "SSP2i_CM11_NoCC_No",
         "SSP2i_CM12_NoCC_No")


# Prm_Ene -----------------------------------------------------------------


Prm_vec <- c("Prm_Ene_Coa_w_CCS", 
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


# Sec_Ene -----------------------------------------------------------------


Sec_vec <- gsub("Prm_Ene", "Sec_Ene_Ele", Prm_vec)


# Fin_Ene -----------------------------------------------------------------


Fin_vec <- c("Fin_Ene_Ele",
             "Fin_Ene_Gas",
             "Fin_Ene_Heat",
             "Fin_Ene_Hyd",
             "Fin_Ene_Liq_Oil",
             "Fin_Ene_Liq_Bio",
             "Fin_Ene_SolidsCoa",
             "Fin_Ene_SolidsBio")


# Emi ---------------------------------------------------------------------


Emi_vec<- c("Emi_CO2_AFO",
            "Emi_CO2_Ene_Sup",
            "Emi_CO2_Ene_Dem",
            "Emi_CO2_Ind_Pro",
            "Emi_CO2_Pro_Use",
            "Emi_CO2_Cap_and_Rem")


# Rem ---------------------------------------------------------------------


Rem_vec <- c("Car_Rem_Bio",
             "Car_Rem_Bio_wit_CCS",
             "Car_Rem_Dir_Air_Cap_wit_CCS",
             "Car_Rem_Enh_Wea", 
             "Car_Rem_Frs",
             "Car_Rem_Soi_Car_Seq")

