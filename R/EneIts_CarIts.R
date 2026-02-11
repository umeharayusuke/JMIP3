output_dir <- file.path("..", "output/laboseminar")

CLP <- c("SSP2i_BaU_NoCC_No", "SSP2i_CM15_NoCC_No", "SSP2i_CM7_NoCC_No","SSP2i_CM24_NoCC_No")
CLP <- c("SSP2i_CM13_NoCC_No", "SSP2i_CM15_NoCC_No","SSP2i_CM18_NoCC_No")

vec <- c("Ele_rat_Ele", 
         "Sha_NonBioRen_Ene_Prm_Ene",
         "Ene_Its", 
         "Car_Its")

df <- rgdx.param("JPN_IAMC.gdx", "IAMC_template") %>%
  filter(VEMF %in% vec) %>%
  #filter(YEMF == "2100") %>% 
  #filter(YEMF %in% c("2050","2100")) %>% 
  filter(SCENARIO %in% CLP)

df$SCENARIO <- gsub("SSP2i_BaU_NoCC_No", "BaU", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM13_NoCC_No", "High", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM15_NoCC_No", "Middle", df$SCENARIO)
df$SCENARIO <- gsub("SSP2i_CM18_NoCC_No", "Low", df$SCENARIO)
df$SCENARIO <- factor(df$SCENARIO, levels = c("High", "Middle", "Low"))

df$VEMF <- dplyr::recode(df$VEMF,
                         "Ele_rat_Ele" = "Electrification rate (%)",
                         "Sha_NonBioRen_Ene_Prm_Ene" = "Renewable share (%)",
                         "Ene_Its" = "Energy intensity (PE GJ/GDP US2010$)",
                         "Car_Its" = "Carbon intensity (CO2 Mt/PE EJ)")

g <- df %>% 
  ggplot(aes(x = YEMF,
             y = IAMC_Template,
             group = SCENARIO,
             color = SCENARIO)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))+
  facet_wrap(~ VEMF, ncol = 2, scales = "free_y") +
  ylab(NULL) +   
  theme_1
plot(g)


name  <- name  <- "EI_CI_ER_RS.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white")