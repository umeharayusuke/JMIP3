output_dir <- file.path("..", "output/laboseminar")

CLP <- c("SSP2i_BaU_NoCC_No", "SSP2i_CM13_NoCC_No", "SSP2i_CM15_NoCC_No","SSP2i_CM18_NoCC_No")

vec <- c("Fin_Ene_Ind_Ele", 
         "Fin_Ene_Res_and_Com_Ele",
         "Fin_Ene_Tra_w_bun_Ele", 
         "Fin_Ene_Car_Man_Dir_Air_Cap_Ele")


df <- rgdx.param("JPN_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% vec) %>%
  #filter(YEMF == "2100") %>% 
  filter(YEMF %in% c("2050","2100")) %>% 
  filter(SCENARIO %in% CLP)
#df$SCENARIO <- factor(df$SCENARIO, levels = CLP)

df$VEMF <- gsub("Fin_Ene_Ind_Ele", "Industry", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Res_and_Com_Ele", "Building", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Tra_w_bun_Ele", "Transport", df$VEMF)
df$VEMF <- gsub("Fin_Ene_Car_Man_Dir_Air_Cap_Ele", "DACCS", df$VEMF)


df$SCENARIO <- gsub("SSP2i_BaU_NoCC_No", "BaU", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM13_NoCC_No", "High", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM15_NoCC_No", "Middle", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM18_NoCC_No", "Low", df$SCENARIO)
df$SCENARIO <- factor(df$SCENARIO, levels = c("BaU","High", "Middle", "Low"))


col_ele <- c(
  "Industry" = "#4E79A7",  
  "Building" = "#F28E2B",   
  "Transport" = "#59A14F", 
  "DACCS" = "#B07AA1"       
)
g <- ggplot(data = df) +
  geom_bar(aes(x = SCENARIO, y = IAMC_Template, fill = VEMF),
           stat = "identity", width = 0.7) +
  scale_fill_manual(values = col_ele) +
  ylab("Electricity use in final energy (EJ)")+
  facet_wrap(~ YEMF)+
  theme_1+
  theme(legend.title = element_blank())

plot(g)



name="Fin_Ene_Ele.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)