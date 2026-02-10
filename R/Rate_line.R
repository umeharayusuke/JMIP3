output_dir <- file.path("..", "output/laboseminar")

CLP <- c("SSP2i_BaU_NoCC_No", "SSP2i_CM15_NoCC_No", "SSP2i_CM7_NoCC_No","SSP2i_CM24_NoCC_No")

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


df <- rgdx.param("JPN_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% vec) %>%
  #filter(YEMF == "2100") %>% 
  filter(YEMF %in% c("2050","2100")) %>% 
  filter(SCENARIO %in% CLP)
#df$SCENARIO <- factor(df$SCENARIO, levels = CLP)

df <- df %>%
  mutate(VEMF = case_when(
    VEMF %in% c("Prm_Ene_Coa_w_CCS","Prm_Ene_Coa_wo_CCS",
                "Prm_Ene_Oil_w_CCS","Prm_Ene_Oil_wo_CCS",
                "Prm_Ene_Gas_w_CCS","Prm_Ene_Gas_wo_CCS",
                "Prm_Ene_Bio_w_CCS","Prm_Ene_Bio_wo_CCS",
                "Prm_Ene_Hyd","Prm_Ene_Nuc") ~ "NonVRE",
    
    VEMF %in% c("Prm_Ene_Solar","Prm_Ene_Win") ~ "VRE"
  ))

df$SCENARIO <- gsub("SSP2i_BaU_NoCC_No", "BaU", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM7_NoCC_No", "High", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM15_NoCC_No", "Middle", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM24_NoCC_No", "Low", df$SCENARIO)
df$SCENARIO <- factor(df$SCENARIO, levels = c("BaU","High", "Middle", "Low"))

df_sum <- df %>%
  group_by(SCENARIO, YEMF, VEMF) %>%
  summarise(value = sum(IAMC_Template), .groups = "drop")
df_share <- df_sum %>%
  pivot_wider(names_from = VEMF, values_from = value) %>%
  mutate(share = VRE / (VRE + NonVRE) * 100)

ggplot(df_share,
       aes(x = YEMF,
           y = share,
           color = SCENARIO,
           group = SCENARIO)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  ylab("VRE share (%)") +
  xlab("Year") +
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))+
  theme_bw()
