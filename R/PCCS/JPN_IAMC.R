
# Emission ----------------------------------------------------------------


output_dir <- file.path("../..", "output/CCS_ET")

CLP <- c("SSP2i_CM1_NoCC_No","SSP2i_CM2_NoCC_No","SSP2i_CM3_NoCC_No","SSP2i_CM4_NoCC_No","SSP2i_CM5_NoCC_No","SSP2i_CM6_NoCC_No")
CLP <- c("SSP2i_CM13_NoCC_No","SSP2i_CM14_NoCC_No","SSP2i_CM15_NoCC_No","SSP2i_CM16_NoCC_No","SSP2i_CM17_NoCC_No","SSP2i_CM18_NoCC_No")
CLP <- c("SSP2i_CM19_NoCC_No","SSP2i_CM20_NoCC_No","SSP2i_CM21_NoCC_No","SSP2i_CM22_NoCC_No","SSP2i_CM23_NoCC_No","SSP2i_CM24_NoCC_No")
CLP <- paste0("SSP2i_CM", c(1:24), "_NoCC_No")

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
         "CDR" = "#377EB8")

ylabel <- "CO2 emission (Mt)"

df <- rgdx.param("JPN_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% vec) %>%
  #filter(YEMF == "2100") %>% 
  #filter(YEMF %in% c("2050","2100")) %>% 
  filter(SCENARIO %in% CLP)
df$SCENARIO <- factor(df$SCENARIO, levels = CLP)

df$VEMF <- gsub("Emi_CO2_AFO", "AFOLU", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Ene_Sup", "Energy Supply", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Ene_Dem", "Energy Demand", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Ind_Pro", "Industrial Processes", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Pro_Use", "Product Use", df$VEMF)
df$VEMF <- gsub("Emi_CO2_Cap_and_Rem", "CDR", df$VEMF)

df$SCENARIO <- factor(
  df$SCENARIO,
  levels = c(
    "SSP2i_BaU_NoCC_No",
    paste0("SSP2i_CM", 1:24, "_NoCC_No")
  )
)


g <- ggplot(data = df) +
  geom_bar(mapping = aes(x = YEMF, y = IAMC_Template, fill = VEMF), 
           stat = "identity", width = 0.7) +
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))+
  scale_fill_manual(values = col)+
  ylab(ylabel)+
  facet_wrap(~ SCENARIO,ncol = 6)+
  theme_1+
  theme(legend.title = element_blank())

plot(g)


name="Emi.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# Car_Seq_CCS -------------------------------------------------------------

vec <- c("Car_Seq_CCS_Bio",
         "Car_Seq_CCS_Fos",
         "Car_Seq_CCS_Ind_Pro",
         "Car_Seq_Dir_Air_Cap")

ylabel <- "CCS (Mt)"


df <- rgdx.param("JPN_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% vec) %>%
  #filter(YEMF == "2100") %>% 
  #filter(YEMF %in% c("2050","2100")) %>% 
  filter(SCENARIO %in% CLP)
#df$SCENARIO <- factor(df$SCENARIO, levels = CLP)

#df$VEMF <- gsub("Car_Rem_Bio_wit_CCS", "BECCS", df$VEMF)
#df$VEMF <- gsub("Car_Rem_Bio", "Biochar", df$VEMF)
#df$VEMF <- gsub("Car_Rem_Dir_Air_Cap_wit_CCS", "DACCS", df$VEMF)
#df$VEMF <- gsub("Car_Rem_Soi_Car_Seq", "Soil Carbon", df$VEMF)

df$SCENARIO <- factor(
  df$SCENARIO,
  levels = c(
    "SSP2i_BaU_NoCC_No",
    paste0("SSP2i_CM", 1:24, "_NoCC_No")
  )
)

g <- ggplot(data = df) +
  geom_bar(mapping = aes(x = YEMF, y = IAMC_Template, fill = VEMF), 
           stat = "identity", width = 0.7) +
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))+
  ylab(ylabel)+
  facet_wrap(~ SCENARIO,ncol=6)+
  theme_1+
  theme(legend.title = element_blank())

plot(g)



name="Car_Seq_CCS.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


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
col <- c("Coal|w/o CCS" = "grey50", "Coal|w/ CCS" = "grey30", "Oil|w/o CCS" = "tan3",
         "Oil|w/ CCS" = "sandybrown", "Gas|w/o CCS" = "lightgoldenrod", "Gas|w/ CCS" = "lightgoldenrod3",
         "Hydro" = "lightsteelblue", "Nuclear" = "moccasin", "Solar" = "lightsalmon", "Wind" = "lightskyblue3",
         "Biomass|w/o CCS" = "darkolivegreen2", "Biomass|w/ CCS" = "darkolivegreen4", "Geothermal" = "peru")
ylabel <- "Primary energy (EJ/yr)"
yvec<-c("2050","2100")

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
#df$SCENARIO <- gsub("SSP2i_BaU_NoCC_No", "BaU", df$SCENARIO)

df$SCENARIO <- factor(
  df$SCENARIO,
  levels = c(
    "SSP2i_BaU_NoCC_No",
    paste0("SSP2i_CM", 1:24, "_NoCC_No")
  )
)

g <- ggplot(df,
            aes(x = YEMF,
                y = IAMC_Template,
                fill = VEMF,
                group = VEMF)) +
  geom_area(alpha = 1,
            position = "stack") +
  scale_fill_manual(values = col) +
  ylab(ylabel) +
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))+
  facet_wrap(~SCENARIO,ncol=6) +
  theme_1 +
  theme(
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot(g)


name="Prm_Ene.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# Sec_Ene -----------------------------------------------------------------


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

df <- rgdx.param("JPN_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% vec) %>%
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

g <- ggplot(df,
            aes(x = YEMF,
                y = IAMC_Template,
                fill = VEMF,
                group = VEMF)) +
  geom_area(alpha = 1,
            position = "stack") +
  scale_fill_manual(values = col) +
  ylab(ylabel) +
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))+
  facet_wrap(~SCENARIO,ncol=6) +
  theme_1 +
  theme(
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot(g)



name="Sec_Ene.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# Fin_Ene -----------------------------------------------------------------


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
  filter(SCENARIO %in% CLP)

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
g <- ggplot(df,
            aes(x = YEMF,
                y = IAMC_Template,
                fill = VEMF,
                group = VEMF)) +
  geom_area(alpha = 1,
            position = "stack") +
  scale_fill_manual(values = col) +
  ylab(ylabel) +
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))+
  facet_wrap(~SCENARIO,ncol=6) +
  theme_1 +
  theme(
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot(g)


name <- "Fin_Ene.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# others ------------------------------------------------------------------


thema <- "Gro_Emi_CO2"
thema <- "Emi_CO2"
thema <- "Pop"
thema <- "GDP_MER"
thema <- "Gro_Rem_CO2"
thema <- "Pol_Cos_Cns_Los_rat"
thema <- "Pol_Cos_GDP_Los_rat"
thema <- "Prc_Car"


#CLP <- c("SSP2i_BaU_NoCC_No", "SSP2i_CM15_NoCC_No", "SSP2i_CM7_NoCC_No","SSP2i_CM24_NoCC_No")
#CLP <- c("SSP2i_CM13_NoCC_No", "SSP2i_CM15_NoCC_No","SSP2i_CM18_NoCC_No")

df <- rgdx.param("JPN_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF == thema) %>%
  #filter(YEMF == "2100") %>% 
  filter(SCENARIO %in% CLP)

df$SCENARIO <- factor(
  df$SCENARIO,
  levels = c(
    "SSP2i_BaU_NoCC_No",
    paste0("SSP2i_CM", 1:24, "_NoCC_No")
  )
)

g <- df %>% 
  ggplot(aes(x = YEMF, y = IAMC_Template, group = SCENARIO, color = SCENARIO)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +  
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))+
  #ylab("GDP|MER (billion US$2010/yr)")+
  #ylab("Population (million)")+
  ylab("Carbon Price (US$2010/yr)")+
  ylab("Emissions|CO2 (Mt/yr)")+
  #ylab("Policy Cost|Consumption (%)")+
  #ylab("Policy Cost|GDP (%)")+
  #ylab("Electrification rate (%)")+
  theme_1+
  theme(legend.position = "bottom")

plot(g)

name  <- paste0(thema, ".png")
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)