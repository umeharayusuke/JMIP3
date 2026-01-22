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
setwd("data")

# Pop ----------------------------------------------------------------


thema <- "Pop"

files <- list(
  list(file = "SSP2i_CM1_NoCC_No.gdx", scenario = "CM1"),
  list(file = "SSP2i_CM2_NoCC_No.gdx", scenario = "CM2"),
  list(file = "SSP2i_CM3_NoCC_No.gdx", scenario = "CM3"),
  list(file = "SSP2i_CM4_NoCC_No.gdx", scenario = "CM4"),
  list(file = "SSP2i_CM5_NoCC_No.gdx", scenario = "CM5"),
  list(file = "SSP2i_CM6_NoCC_No.gdx", scenario = "CM6"),
  list(file = "SSP2i_CM7_NoCC_No.gdx", scenario = "CM7"),
  list(file = "SSP2i_CM8_NoCC_No.gdx", scenario = "CM8"),
  list(file = "SSP2i_CM9_NoCC_No.gdx", scenario = "CM9"),
  list(file = "SSP2i_CM10_NoCC_No.gdx", scenario = "CM10"),
  list(file = "SSP2i_CM11_NoCC_No.gdx", scenario = "CM11"),
  list(file = "SSP2i_CM12_NoCC_No.gdx", scenario = "CM12"),
  list(file = "SSP2i_CM13_NoCC_No.gdx", scenario = "CM13"),
  list(file = "SSP2i_CM14_NoCC_No.gdx", scenario = "CM14"),
  list(file = "SSP2i_CM15_NoCC_No.gdx", scenario = "CM15"),
  list(file = "SSP2i_CM16_NoCC_No.gdx", scenario = "CM16"),
  list(file = "SSP2i_CM17_NoCC_No.gdx", scenario = "CM17"),
  list(file = "SSP2i_CM18_NoCC_No.gdx", scenario = "CM18"),
  list(file = "SSP2i_CM19_NoCC_No.gdx", scenario = "CM19"),
  list(file = "SSP2i_CM20_NoCC_No.gdx", scenario = "CM20"),
  list(file = "SSP2i_CM21_NoCC_No.gdx", scenario = "CM21"),
  list(file = "SSP2i_CM22_NoCC_No.gdx", scenario = "CM22"),
  list(file = "SSP2i_CM23_NoCC_No.gdx", scenario = "CM23"),
  list(file = "SSP2i_CM24_NoCC_No.gdx", scenario = "CM24")
)

df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "IAMC_template")
  df_temp <- gdx_data %>%
    filter(VEMF == thema) %>%
    rename(Year = "Y") %>%
    rename(CP = "IAMC_template") %>%
    mutate(VEMF = file_info$scenario)
  
  df <- rbind(df, df_temp)
}

df$VEMF <- factor(df$VEMF, levels = paste0("CM", 1:24))


g1 <- df %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  ggplot(aes(x = Year, y = CP, group = VEMF, color = VEMF)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +  
  scale_x_discrete(
    breaks = c("2010", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
  ) +
  scale_y_continuous(
    name = expression(paste("Population (Million)")),
  ) +
  theme_1 +
  theme(
    legend.position = "right",
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

plot(g1)


output_dir <- file.path("..", "output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

ggsave(
  filename = file.path(output_dir, "POP.png"),
  plot = g1,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
# GDP_PPP ----------------------------------------------------------------
thema <- "GDP_PPP"


files <- list(
  list(file = "SSP2i_CM1_NoCC_No.gdx", scenario = "CM1"),
  list(file = "SSP2i_CM2_NoCC_No.gdx", scenario = "CM2"),
  list(file = "SSP2i_CM3_NoCC_No.gdx", scenario = "CM3"),
  list(file = "SSP2i_CM4_NoCC_No.gdx", scenario = "CM4"),
  list(file = "SSP2i_CM5_NoCC_No.gdx", scenario = "CM5"),
  list(file = "SSP2i_CM6_NoCC_No.gdx", scenario = "CM6"),
  list(file = "SSP2i_CM7_NoCC_No.gdx", scenario = "CM7"),
  list(file = "SSP2i_CM8_NoCC_No.gdx", scenario = "CM8"),
  list(file = "SSP2i_CM9_NoCC_No.gdx", scenario = "CM9"),
  list(file = "SSP2i_CM10_NoCC_No.gdx", scenario = "CM10"),
  list(file = "SSP2i_CM11_NoCC_No.gdx", scenario = "CM11"),
  list(file = "SSP2i_CM12_NoCC_No.gdx", scenario = "CM12"),
  list(file = "SSP2i_CM13_NoCC_No.gdx", scenario = "CM13"),
  list(file = "SSP2i_CM14_NoCC_No.gdx", scenario = "CM14"),
  list(file = "SSP2i_CM15_NoCC_No.gdx", scenario = "CM15"),
  list(file = "SSP2i_CM16_NoCC_No.gdx", scenario = "CM16"),
  list(file = "SSP2i_CM17_NoCC_No.gdx", scenario = "CM17"),
  list(file = "SSP2i_CM18_NoCC_No.gdx", scenario = "CM18"),
  list(file = "SSP2i_CM19_NoCC_No.gdx", scenario = "CM19"),
  list(file = "SSP2i_CM20_NoCC_No.gdx", scenario = "CM20"),
  list(file = "SSP2i_CM21_NoCC_No.gdx", scenario = "CM21"),
  list(file = "SSP2i_CM22_NoCC_No.gdx", scenario = "CM22"),
  list(file = "SSP2i_CM23_NoCC_No.gdx", scenario = "CM23"),
  list(file = "SSP2i_CM24_NoCC_No.gdx", scenario = "CM24")
)

df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "IAMC_template")
  df_temp <- gdx_data %>%
    filter(VEMF == thema) %>%
    rename(Year = "Y") %>%
    rename(CP = "IAMC_template") %>%
    mutate(VEMF = file_info$scenario)
  df <- rbind(df, df_temp)
}

df$VEMF <- factor(df$VEMF, levels = paste0("CM", 1:24))


g2 <- df %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  ggplot(aes(x = Year, y = CP, group = VEMF, color = VEMF)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +  
  scale_x_discrete(
    breaks = c("2010", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
  ) +
  scale_y_continuous(
    name = expression(paste("GDP_PPP (billion US$2017)")),
  ) +
  theme_1 +
  theme(
    legend.position = "right",
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

plot(g2)

ggsave(
  filename = file.path(output_dir, "GDP.png"),
  plot = g2,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# Emission ----------------------------------------------------------------


thema <- "Emi_CO2_Ene_and_Ind_Pro"
#thema <- "Gro_Rem_CO2"
#thema <- "Gro_Emi_CO2"
thema <- "Emi_CO2"


files <- list(
  list(file = "SSP2i_CM1_NoCC_No.gdx", scenario = "CM7"),
  list(file = "SSP2i_CM2_NoCC_No.gdx", scenario = "CM8"),
  list(file = "SSP2i_CM3_NoCC_No.gdx", scenario = "CM9"),
  list(file = "SSP2i_CM4_NoCC_No.gdx", scenario = "CM10"),
  list(file = "SSP2i_CM5_NoCC_No.gdx", scenario = "CM11"),
  list(file = "SSP2i_CM6_NoCC_No.gdx", scenario = "CM12"),
  list(file = "SSP2i_CM7_NoCC_No.gdx", scenario = "CM1"),
  list(file = "SSP2i_CM8_NoCC_No.gdx", scenario = "CM2"),
  list(file = "SSP2i_CM9_NoCC_No.gdx", scenario = "CM3"),
  list(file = "SSP2i_CM10_NoCC_No.gdx", scenario = "CM4"),
  list(file = "SSP2i_CM11_NoCC_No.gdx", scenario = "CM5"),
  list(file = "SSP2i_CM12_NoCC_No.gdx", scenario = "CM6"),
  list(file = "SSP2i_CM13_NoCC_No.gdx", scenario = "CM13"),
  list(file = "SSP2i_CM14_NoCC_No.gdx", scenario = "CM14"),
  list(file = "SSP2i_CM15_NoCC_No.gdx", scenario = "CM15"),
  list(file = "SSP2i_CM16_NoCC_No.gdx", scenario = "CM16"),
  list(file = "SSP2i_CM17_NoCC_No.gdx", scenario = "CM17"),
  list(file = "SSP2i_CM18_NoCC_No.gdx", scenario = "CM18"),
  list(file = "SSP2i_CM19_NoCC_No.gdx", scenario = "CM19"),
  list(file = "SSP2i_CM20_NoCC_No.gdx", scenario = "CM20"),
  list(file = "SSP2i_CM21_NoCC_No.gdx", scenario = "CM21"),
  list(file = "SSP2i_CM22_NoCC_No.gdx", scenario = "CM22"),
  list(file = "SSP2i_CM23_NoCC_No.gdx", scenario = "CM23"),
  list(file = "SSP2i_CM24_NoCC_No.gdx", scenario = "CM24")
)

df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "IAMC_template")
  df_temp <- gdx_data %>%
    filter(VEMF == thema) %>%
    rename(Year = "Y") %>%
    rename(CP = "IAMC_template") %>%
    mutate(VEMF = file_info$scenario)
  
  df <- rbind(df, df_temp)
}

df$VEMF <- factor(df$VEMF, levels = paste0("CM", 1:24))



g1 <- df %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  ggplot(aes(x = Year, y = CP, group = VEMF, color = VEMF)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +  
  scale_x_discrete(
    breaks = c("2020",  "2040", "2060",  "2080","2100")
  ) +
  scale_y_continuous(name = expression(paste("Emi_CO2 (Mt)")))
plot(g1)



name  <- paste0(thema, ".png")
ggsave(
  filename = file.path(output_dir, name),
  plot = g1,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
# carbon price ------------------------------------------------------------

thema <- "Prc_Car"

files <- list(
  list(file = "SSP2i_CM1_NoCC_No.gdx", scenario = "CM7"),
  list(file = "SSP2i_CM2_NoCC_No.gdx", scenario = "CM8"),
  list(file = "SSP2i_CM3_NoCC_No.gdx", scenario = "CM9"),
  list(file = "SSP2i_CM4_NoCC_No.gdx", scenario = "CM10"),
  list(file = "SSP2i_CM5_NoCC_No.gdx", scenario = "CM11"),
  list(file = "SSP2i_CM6_NoCC_No.gdx", scenario = "CM12"),
  list(file = "SSP2i_CM7_NoCC_No.gdx", scenario = "CM1"),
  list(file = "SSP2i_CM8_NoCC_No.gdx", scenario = "CM2"),
  list(file = "SSP2i_CM9_NoCC_No.gdx", scenario = "CM3"),
  list(file = "SSP2i_CM10_NoCC_No.gdx", scenario = "CM4"),
  list(file = "SSP2i_CM11_NoCC_No.gdx", scenario = "CM5"),
  list(file = "SSP2i_CM12_NoCC_No.gdx", scenario = "CM6"),
  list(file = "SSP2i_CM13_NoCC_No.gdx", scenario = "CM13"),
  list(file = "SSP2i_CM14_NoCC_No.gdx", scenario = "CM14"),
  list(file = "SSP2i_CM15_NoCC_No.gdx", scenario = "CM15"),
  list(file = "SSP2i_CM16_NoCC_No.gdx", scenario = "CM16"),
  list(file = "SSP2i_CM17_NoCC_No.gdx", scenario = "CM17"),
  list(file = "SSP2i_CM18_NoCC_No.gdx", scenario = "CM18"),
  list(file = "SSP2i_CM19_NoCC_No.gdx", scenario = "CM19"),
  list(file = "SSP2i_CM20_NoCC_No.gdx", scenario = "CM20"),
  list(file = "SSP2i_CM21_NoCC_No.gdx", scenario = "CM21"),
  list(file = "SSP2i_CM22_NoCC_No.gdx", scenario = "CM22"),
  list(file = "SSP2i_CM23_NoCC_No.gdx", scenario = "CM23"),
  list(file = "SSP2i_CM24_NoCC_No.gdx", scenario = "CM24")
)

df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "IAMC_template")
  df_temp <- gdx_data %>%
    filter(VEMF == thema) %>%
    rename(Year = "Y") %>%
    rename(CP = "IAMC_template") %>%
    mutate(VEMF = file_info$scenario)
  
  df <- rbind(df, df_temp)
}

df$VEMF <- factor(df$VEMF, levels = paste0("CM", 1:24))

df <- df %>%
  mutate(
    scen_num = as.numeric(gsub("CM", "", VEMF)),
    scen_group = paste0(
      "CM", (ceiling(scen_num / 6) - 1) * 6 + 1,
      "-CM", ceiling(scen_num / 6) * 6
    )
  ) 
df$scen_group <- gsub("CM1-CM6", "20%by2050(CM1-6)", df$scen_group)
df$scen_group <- gsub("CM7-CM12", "0%by2050(CM7-12)", df$scen_group)
df$scen_group <- gsub("CM13-CM18", "-5%by2050(CM13-18)", df$scen_group)
df$scen_group <- gsub("CM19-CM24", "-20%by2050(CM19-24)", df$scen_group)

df$scen_group <- factor(
  df$scen_group,
  levels = c(
    "20%by2050(CM1-6)",
    "0%by2050(CM7-12)",
    "-5%by2050(CM13-18)",
    "-20%by2050(CM19-24)"
  )
)

g1 <- df %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  ggplot(aes(x = Year, y = CP, group = VEMF, color = VEMF)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +  
  scale_x_discrete(
    breaks = c("2010", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
  ) +
  scale_y_continuous(
    name = expression(paste("Carbon price (US$)")),
  ) +
  facet_wrap(~ scen_group, scales = "free_y") 


plot(g1)

name  <- paste0(thema, ".png")
ggsave(
  filename = file.path(output_dir, name),
  plot = g1,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

df <- df %>%
  mutate(
    scen_num = as.numeric(gsub("CM", "", VEMF)),
    mod6 = scen_num %% 6,
    mod6 = ifelse(mod6 == 0, 6, mod6),   
    scen_group = paste0("mod", mod6)
  )

df <- df %>%
  mutate(
    scen_group = factor(
      scen_group,
      levels = paste0("mod", 1:6),
      labels = c(
        "20%by2100(CM1,7,13,19)",
        "0%by2100(CM2,8,14,20)",
        "-5%by2100(CM3,9,15,21)",
        "-20%by2100(CM4,10,16,22)",
        "-50%by2100(CM5,11,17,23)",
        "-100%by2050(CM6,12,18,24)"
      )
    )
  )

g1 <- df %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  ggplot(aes(x = Year, y = CP, group = VEMF, color = VEMF)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +  
  scale_x_discrete(
    breaks = c("2010", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
  ) +
  scale_y_continuous(
    name = expression(paste("Carbon price (US$)"))
  ) +
  facet_wrap(~ scen_group, scales = "free_y")
plot(g1)

name  <- paste0(thema, "_mod6_.png")

ggsave(
  filename = file.path(output_dir, name),
  plot = g1,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
# Car Rem -----------------------------------------------------------------


year <- 2100

load_and_format_data <- function(gdx_file, scenario_name,  year) {
  df <- rgdx.param(gdx_file, "IAMC_template") %>%
    filter(Y == year) %>%
    filter(VEMF %in% c("Car_Rem_Bio",
                       "Car_Rem_Bio_wit_CCS", 
                       "Car_Rem_Dir_Air_Cap_wit_CCS", 
                       "Car_Rem_Enh_Wea", 
                       "Car_Rem_Frs",
                       "Car_Rem_Soi_Car_Seq"))
  
  df$VEMF <- gsub("Car_Rem_Bio_wit_CCS", "BECCS", df$VEMF)
  df$VEMF <- gsub("Car_Rem_Bio", "Biochar", df$VEMF)
  df$VEMF <- gsub("Car_Rem_Dir_Air_Cap_wit_CCS", "DACCS", df$VEMF)
  df$VEMF <- gsub("Car_Rem_Enh_Wea", "Enhanced Weather", df$VEMF)
  df$VEMF <- gsub("Car_Rem_Frs", "Afforestation", df$VEMF)
  df$VEMF <- gsub("Car_Rem_Soi_Car_Seq", "Soil Carbon", df$VEMF)
  
  
  df$scenario <- scenario_name
  
  return(df)
}

scenarios <- list(
  list(gdx_file = "SSP2i_CM19_NoCC_No.gdx", scenario_name = "CM1"),
  list(gdx_file = "SSP2i_CM20_NoCC_No.gdx", scenario_name = "CM2"),
  list(gdx_file = "SSP2i_CM21_NoCC_No.gdx", scenario_name = "CM3"),
  list(gdx_file = "SSP2i_CM22_NoCC_No.gdx", scenario_name = "CM4"),
  list(gdx_file = "SSP2i_CM23_NoCC_No.gdx", scenario_name = "CM5"),
  list(gdx_file = "SSP2i_CM24_NoCC_No.gdx", scenario_name = "CM6")
  
  
)



all_data <- purrr::map_dfr(scenarios, function(scenario) {
  load_and_format_data(scenario$gdx_file, scenario$scenario_name,  year)
})

all_data$scenario <- factor(all_data$scenario, levels = c("CM1", "CM2", "CM3", "CM4", "CM5", "CM6"))

color <- c(
  "BECCS" = "#4DAF4A",          
  "Biochar" = "#E69F00",       
  "Soil Carbon" = "#A65628",    
  "Afforestation" = "#1B7837",  
  "Enhanced Weather" = "#377EB8",
  "DACCS" = "#984EA3"           
)

all_data$VEMF <- factor(all_data$VEMF, levels = c("BECCS", "Enhanced Weather", "Biochar", "Soil Carbon", "DACCS", "Afforestation"))


g7 <- ggplot(data = all_data) +
  geom_bar(aes(x = scenario, y = IAMC_template / 1000, fill = VEMF), stat = "identity", position = "stack", width = 0.7) +
  scale_y_continuous(name = expression(paste("Carbon Sequestration in 2100 (Gt)"))) + 
  #facet_wrap(~REMF, ncol = 3, scales = "free_y") +  
  scale_fill_manual(
    values = color, 
    breaks = c("BECCS", 
               "Enhanced Weather",
               "Biochar", 
               "Soil Carbon",
               "DACCS", 
               "Afforestation"), 
    guide = guide_legend(reverse = FALSE)
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  theme_1  +
  labs(fill = NULL) 

print(g7)




load_and_format_data <- function(gdx_file, scenario_name,  years) {
  df <- rgdx.param(gdx_file, "IAMC_template") %>%
    filter(Y %in% years) %>%
    filter(VEMF %in% c("Car_Rem_Bio",
                       "Car_Rem_Bio_wit_CCS", 
                       "Car_Rem_Dir_Air_Cap_wit_CCS", 
                       "Car_Rem_Enh_Wea", 
                       "Car_Rem_Frs",
                       "Car_Rem_Soi_Car_Seq"))
  
  df$VEMF <- recode(df$VEMF,
                    "Car_Rem_Bio_wit_CCS" = "BECCS",
                    "Car_Rem_Bio" = "Biochar",
                    "Car_Rem_Dir_Air_Cap_wit_CCS" = "DACCS",
                    "Car_Rem_Enh_Wea" = "Enhanced Weather",
                    "Car_Rem_Frs" = "Afforestation",
                    "Car_Rem_Soi_Car_Seq" = "Soil Carbon")
  
  df$scenario <- scenario_name
  
  return(df)
}


scenarios <- list(
  list(gdx_file = "SSP2i_CM1_NoCC_No.gdx", scenario_name = "CM7"),
  list(gdx_file = "SSP2i_CM2_NoCC_No.gdx", scenario_name = "CM8"),
  list(gdx_file = "SSP2i_CM3_NoCC_No.gdx", scenario_name = "CM9"),
  list(gdx_file = "SSP2i_CM4_NoCC_No.gdx", scenario_name = "CM10"),
  list(gdx_file = "SSP2i_CM5_NoCC_No.gdx", scenario_name = "CM11"),
  list(gdx_file = "SSP2i_CM6_NoCC_No.gdx", scenario_name = "CM12"),
  list(gdx_file = "SSP2i_CM7_NoCC_No.gdx", scenario_name = "CM1"),
  list(gdx_file = "SSP2i_CM8_NoCC_No.gdx", scenario_name = "CM2"),
  list(gdx_file = "SSP2i_CM9_NoCC_No.gdx", scenario_name = "CM3"),
  list(gdx_file = "SSP2i_CM10_NoCC_No.gdx", scenario_name = "CM4"),
  list(gdx_file = "SSP2i_CM11_NoCC_No.gdx", scenario_name = "CM5"),
  list(gdx_file = "SSP2i_CM12_NoCC_No.gdx", scenario_name = "CM6"),
  list(gdx_file = "SSP2i_CM13_NoCC_No.gdx", scenario_name = "CM13"),
  list(gdx_file = "SSP2i_CM14_NoCC_No.gdx", scenario_name = "CM14"),
  list(gdx_file = "SSP2i_CM15_NoCC_No.gdx", scenario_name = "CM15"),
  list(gdx_file = "SSP2i_CM16_NoCC_No.gdx", scenario_name = "CM16"),
  list(gdx_file = "SSP2i_CM17_NoCC_No.gdx", scenario_name = "CM17"),
  list(gdx_file = "SSP2i_CM18_NoCC_No.gdx", scenario_name = "CM18"),
  list(gdx_file = "SSP2i_CM19_NoCC_No.gdx", scenario_name = "CM19"),
  list(gdx_file = "SSP2i_CM20_NoCC_No.gdx", scenario_name = "CM20"),
  list(gdx_file = "SSP2i_CM21_NoCC_No.gdx", scenario_name = "CM21"),
  list(gdx_file = "SSP2i_CM22_NoCC_No.gdx", scenario_name = "CM22"),
  list(gdx_file = "SSP2i_CM23_NoCC_No.gdx", scenario_name = "CM23"),
  list(gdx_file = "SSP2i_CM24_NoCC_No.gdx", scenario_name = "CM24")
)

years <- seq(2020, 2100, by = 1)

all_data <- purrr::map_dfr(scenarios, function(scenario) {
  load_and_format_data(scenario$gdx_file, scenario$scenario_name,  years)
})

all_data_sum <- all_data %>%
  group_by(scenario, VEMF) %>%
  summarise(total_cseq = sum(IAMC_template) / 1000, .groups = "drop")

all_data_sum$scenario <- factor(all_data_sum$scenario,
                                levels = paste0("CM", 1:24))


all_data_sum$VEMF <- factor(all_data_sum$VEMF,
                            levels = c("BECCS", "Enhanced Weather", "Biochar", 
                                       "Soil Carbon", "DACCS", "Afforestation"))

color <- c(
  "BECCS" = "#4DAF4A",
  "Enhanced Weather" = "#377EB8",
  "Biochar" = "#E69F00",
  "Soil Carbon" = "#A65628",
  "DACCS" = "#984EA3",
  "Afforestation" = "#1B7837"
)

g7 <- ggplot(data = all_data_sum) +
  geom_bar(aes(x = scenario, y = total_cseq, fill = VEMF),
           stat = "identity", position = "stack", width = 0.7) +
  scale_y_continuous(name = expression(paste("Cumulative Carbon Sequestration (2020–2100, Gt)"))) +
  scale_fill_manual(values = color) 

print(g7)

name  <- "CDR.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g7,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
# Primary energy ----------------------------------------------------------


scenarios <- list(
  list(gdx_file = "SSP2i_CM1_NoCC_No.gdx", scenario_name = "CM7"),
  list(gdx_file = "SSP2i_CM2_NoCC_No.gdx", scenario_name = "CM8"),
  list(gdx_file = "SSP2i_CM3_NoCC_No.gdx", scenario_name = "CM9"),
  list(gdx_file = "SSP2i_CM4_NoCC_No.gdx", scenario_name = "CM10"),
  list(gdx_file = "SSP2i_CM5_NoCC_No.gdx", scenario_name = "CM11"),
  list(gdx_file = "SSP2i_CM6_NoCC_No.gdx", scenario_name = "CM12"),
  list(gdx_file = "SSP2i_CM7_NoCC_No.gdx", scenario_name = "CM1"),
  list(gdx_file = "SSP2i_CM8_NoCC_No.gdx", scenario_name = "CM2"),
  list(gdx_file = "SSP2i_CM9_NoCC_No.gdx", scenario_name = "CM3"),
  list(gdx_file = "SSP2i_CM10_NoCC_No.gdx", scenario_name = "CM4"),
  list(gdx_file = "SSP2i_CM11_NoCC_No.gdx", scenario_name = "CM5"),
  list(gdx_file = "SSP2i_CM12_NoCC_No.gdx", scenario_name = "CM6"),
  list(gdx_file = "SSP2i_CM13_NoCC_No.gdx", scenario_name = "CM13"),
  list(gdx_file = "SSP2i_CM14_NoCC_No.gdx", scenario_name = "CM14"),
  list(gdx_file = "SSP2i_CM15_NoCC_No.gdx", scenario_name = "CM15"),
  list(gdx_file = "SSP2i_CM16_NoCC_No.gdx", scenario_name = "CM16"),
  list(gdx_file = "SSP2i_CM17_NoCC_No.gdx", scenario_name = "CM17"),
  list(gdx_file = "SSP2i_CM18_NoCC_No.gdx", scenario_name = "CM18"),
  list(gdx_file = "SSP2i_CM19_NoCC_No.gdx", scenario_name = "CM19"),
  list(gdx_file = "SSP2i_CM20_NoCC_No.gdx", scenario_name = "CM20"),
  list(gdx_file = "SSP2i_CM21_NoCC_No.gdx", scenario_name = "CM21"),
  list(gdx_file = "SSP2i_CM22_NoCC_No.gdx", scenario_name = "CM22"),
  list(gdx_file = "SSP2i_CM23_NoCC_No.gdx", scenario_name = "CM23"),
  list(gdx_file = "SSP2i_CM24_NoCC_No.gdx", scenario_name = "CM24")
)


year <- 2100 
textsize <- 15  

region_code <- "JPN"

process_data <- function(data, region_code, year) {
  df2 <- data %>% filter(REMF %in% region_code)
  df3 <- df2 %>% filter(Y == year)
  df4 <- df3 %>% filter(VEMF %in% c("Prm_Ene_Coa_w_CCS", "Prm_Ene_Coa_wo_CCS", 
                                    "Prm_Ene_Gas_w_CCS", "Prm_Ene_Gas_wo_CCS", 
                                    "Prm_Ene_Oil_w_CCS", "Prm_Ene_Oil_wo_CCS", 
                                    "Prm_Ene_Hyd", 
                                    "Prm_Ene_Solar", "Prm_Ene_Win", 
                                    "Prm_Ene_Nuc", 
                                    "Prm_Ene_Bio_w_CCS", "Prm_Ene_Bio_wo_CCS"))
  
  df4$VEMF <- gsub("Prm_Ene_Hyd", "Hydro", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Solar", "Solar", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Win", "Wind", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Nuc", "Nuclear", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Coa_w_CCS|Prm_Ene_Coa_wo_CCS|Prm_Ene_Oil_w_CCS|Prm_Ene_Oil_wo_CCS|Prm_Ene_Gas_w_CCS|Prm_Ene_Gas_wo_CCS", 
                   "Fossil Fuels", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Bio_w_CCS|Prm_Ene_Bio_wo_CCS", 
                   "Biomass", df4$VEMF)
  return(df4)
}

df_list <- lapply(scenarios, function(s) {
  
  data <- rgdx.param(s$gdx_file, "IAMC_template")
  
  df <- process_data(data, region_code, year)
  
  df$scenario <- s$scenario_name
  
  return(df)
})


df5 <- do.call(rbind, df_list)


df5$scenario <- factor(df5$scenario,
                                levels = paste0("CM", 1:24))

g1 <- ggplot(data = df5) +
  geom_bar(mapping = aes(x = scenario, y = IAMC_template, fill = VEMF), 
           stat = "identity", width = 0.7) +
  ylab("Primary energy \n in 2100 (EJ/yr)") +
  scale_fill_manual(
    values = c(
      "Biomass" = "darkolivegreen2", 
      "Fossil Fuels" = "gray60",  
      "Hydro" = "lightsteelblue", 
      "Nuclear" = "moccasin",  "Solar" = "lightsalmon", "Wind" = "lightskyblue3"
    ),
    breaks = c("Biomass", "Fossil Fuels",
               "Hydro", "Nuclear",  "Solar", "Wind")
  ) +
  labs(fill = "") +
  theme_1

plot(g1)



name  <- "Primary_energy.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g1,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
# Power generation --------------------------------------------------------


prm_vector <- c("Prm_Ene_Coa_w_CCS", "Prm_Ene_Coa_wo_CCS", "Prm_Ene_Gas_w_CCS", "Prm_Ene_Gas_wo_CCS", "Prm_Ene_Oil_w_CCS", "Prm_Ene_Oil_wo_CCS", 
                "Prm_Ene_Hyd", "Prm_Ene_Solar", "Prm_Ene_Win",  "Prm_Ene_Nuc", "Prm_Ene_Bio_w_CCS", "Prm_Ene_Bio_wo_CCS")
sec_vector <- gsub("Prm_Ene", "Sec_Ene_Ele", prm_vector)

region_code <- "JPN"



process_data <- function(gdx_file, scenario, region_code, year, sec_vector) {
  df <- rgdx.param(gdx_file, "IAMC_template") %>%
    filter(REMF %in% region_code) %>%
    filter(Y == year) %>%
    filter(VEMF %in% sec_vector)
  
  df$scenario <- scenario
  
  df$VEMF <- gsub("Sec_Ene_Ele_Hyd", "Hydro", df$VEMF)
  df$VEMF <- gsub("Sec_Ene_Ele_Solar", "Solar", df$VEMF)
  df$VEMF <- gsub("Sec_Ene_Ele_Win", "Wind", df$VEMF)
  df$VEMF <- gsub("Sec_Ene_Ele_Nuc", "Nuclear", df$VEMF)
  
  df$VEMF <- gsub(
    "Sec_Ene_Ele_Gas_w_CCS|Sec_Ene_Ele_Gas_wo_CCS|Sec_Ene_Ele_Oil_w_CCS|Sec_Ene_Ele_Oil_wo_CCS|Sec_Ene_Ele_Coa_w_CCS|Sec_Ene_Ele_Coa_wo_CCS", 
    "Fossil Fuels", df$VEMF
  )
  
  df$VEMF <- gsub(
    "Sec_Ene_Ele_Bio_w_CCS|Sec_Ene_Ele_Bio_wo_CCS", 
    "Biomass", df$VEMF
  )
  
  return(df)
}

df_all <- data.frame()

for (s in scenarios) {
  df_temp <- process_data(
    gdx_file   = s$gdx_file,
    scenario   = s$scenario_name,
    region_code = region_code,
    year       = 2100,
    sec_vector = sec_vector
  )
  
  df_all <- rbind(df_all, df_temp)
}

df_all$scenario <- factor(df_all$scenario,
                       levels = paste0("CM", 1:24))

g2 <- ggplot(data = df_all) +
  geom_bar(aes(x = scenario, y = IAMC_template, fill = VEMF),
           stat = "identity", width = 0.7) +
  ylab("Power generation \n in 2100 (EJ/yr)") +
  scale_fill_manual(
    values = c(
      "Biomass" = "darkolivegreen2", 
      "Fossil Fuels" = "grey50", 
      "Geothermal" = "peru",
      "Hydro" = "lightsteelblue", 
      "Nuclear" = "moccasin", 
      "Solar" = "lightsalmon", 
      "Wind" = "lightskyblue3"
    ),
    breaks = c("Biomass","Fossil Fuels","Geothermal","Hydro","Nuclear","Solar","Wind")
  ) +
  labs(fill = "") +
  theme_1

plot(g2)

name  <- "Power_generation.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g2,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
# Final energy source -----------------------------------------------------

year <- 2100

color <- c( 
  "Coal"="grey70",
  "Oil"="sandybrown",
  "Gas"="moccasin",
  "Biomass"="#A9D65D",
  "Biofuel"="#DBFF70",
  "Electricity"="lightsteelblue",
  "Heat"="salmon",
  "Hydrogen"="thistle2"
)

load_and_format_data <- function(gdx_file, scenario_name, region_code, year) {
  df <- rgdx.param(gdx_file, "IAMC_template") %>%
    filter(REMF %in% region_code) %>%
    filter(Y == year) %>%
    filter(VEMF %in% c("Fin_Ene_Ele",
                       "Fin_Ene_Gas",
                       "Fin_Ene_Heat",
                       "Fin_Ene_Hyd",
                       "Fin_Ene_Liq_Oil",
                       "Fin_Ene_Liq_Bio",
                       "Fin_Ene_SolidsCoa",
                       "Fin_Ene_SolidsBio"))
  
  df$VEMF <- gsub("Fin_Ene_Ele", "Electricity", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Gas", "Gas", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Heat", "Heat", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Hyd", "Hydrogen", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Liq_Oil", "Oil", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Liq_Bio", "Biofuel", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_SolidsCoa", "Coal", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_SolidsBio", "Biomass", df$VEMF)
  
  df$scenario <- scenario_name
  
  return(df)
}


region_code <- "JPN"

all_data <- purrr::map_dfr(scenarios, function(scenario) {
  load_and_format_data(scenario$gdx_file, scenario$scenario_name, region_code, year)
})


df_all_sum <- all_data %>%
  group_by(scenario, REMF) %>%
  summarise(total_emission = sum(IAMC_template, na.rm = TRUE))


all_data$scenario <- factor(all_data$scenario,
                            levels = paste0("CM", 1:24))

g3 <- ggplot(data = all_data) +
  geom_bar(aes(x = scenario, y = IAMC_template, fill = VEMF), stat = "identity", position = "stack", width = 0.7)+
  ylab("Final Energy \n in 2100 (EJ/yr) ") + 
  #facet_wrap(~REMF, ncol = 3, scales = "free_y") + 
  scale_fill_manual(
    values = color, 
    breaks = c("Biofuel",
               "Biomass",
               "Coal",
               "Electricity",
               "Gas",
               "Heat",
               "Hydrogen",
               "Oil"
    ), 
    guide = guide_legend(reverse = FALSE)
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  
  labs(fill = "") +  
  theme_1  


print(g3)


name  <- "final_energy.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g3,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
# Emi_CO2 --------------------------------------------------------------
year <- 2100
color <- c("AFOLU" = "#FC8D62",
           "Energy Supply" = "#66C2A5",
           "Energy Demand" = "#8DA0CB",
           "Industrial Processes" = "#984EA3",
           "Product Use" = "#FFFF33",
           "CDR(NonAff and NonBECCS)" = "#377EB8")

load_and_format_data <- function(gdx_file, scenario_name, year) {
  df <- rgdx.param(gdx_file, "IAMC_template") %>%
    filter(Y == year) %>%
    filter(VEMF %in% c("Emi_CO2_AFO",
                       "Emi_CO2_Ene_Sup", 
                       "Emi_CO2_Ene_Dem", 
                       "Emi_CO2_Ind_Pro",
                       "Emi_CO2_Pro_Use",
                       "Emi_CO2_Cap_and_Rem"))
  
  df$VEMF <- gsub("Emi_CO2_AFO", "AFOLU", df$VEMF)
  df$VEMF <- gsub("Emi_CO2_Ene_Sup", "Energy Supply", df$VEMF)
  df$VEMF <- gsub("Emi_CO2_Ene_Dem", "Energy Demand", df$VEMF)
  df$VEMF <- gsub("Emi_CO2_Ind_Pro", "Industrial Processes", df$VEMF)
  df$VEMF <- gsub("Emi_CO2_Pro_Use", "Product Use", df$VEMF)
  df$VEMF <- gsub("Emi_CO2_Cap_and_Rem", "CDR(NonAff and NonBECCS)", df$VEMF)
  df$scenario <- scenario_name
  return(df)
}



all_data <- purrr::map_dfr(scenarios, function(scenario) {
  load_and_format_data(scenario$gdx_file, scenario$scenario_name, year)
})


df_all_sum <- all_data %>%
  group_by(scenario, REMF) %>%
  summarise(total_emission = sum(IAMC_template, na.rm = TRUE))


all_data$scenario <- factor(all_data$scenario,
                                levels = paste0("CM", 1:24))

g6 <- ggplot(data = all_data) +
  geom_bar(aes(x = scenario, y = IAMC_template, fill = VEMF), stat = "identity", position = "stack", width = 0.7) +
  scale_y_continuous(name = expression(paste({CO[2]}," emission in 2100 (Mt)"))) + 
  facet_wrap(~REMF, ncol = 3, scales = "free_y") +  
  scale_fill_manual(
    values = color, 
    breaks = c("Energy Demand","Energy Supply","Industrial Processes", "Product Use", "AFOLU","CDR(NonAff and NonBECCS)"), 
    guide = guide_legend(reverse = FALSE) 
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  
  theme_1  


g6 <- g6 + geom_point(data = df_all_sum, 
                      aes(x = scenario, y = total_emission), 
                      size = 1, shape = 19, color = "black") +  
  guides(color = "none")  

print(g6)

name  <- "Emission.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g6,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# ref ---------------------------------------------------------------------



thema <- "Emi_CO2_Ene_and_Ind_Pro"
#thema <- "Gro_Rem_CO2"
#thema <- "Gro_Emi_CO2"
thema <- "Emi_CO2"
thema <- "Gro_Rem_CO2"


df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "IAMC_template")
  df_temp <- gdx_data %>%
    filter(VEMF == thema) %>%
    rename(Year = "Y") %>%
    rename(CP = "IAMC_template") %>%
    mutate(VEMF = file_info$scenario)
  
  df <- rbind(df, df_temp)
}

df$VEMF <- factor(df$VEMF, levels = paste0("CM", 1:24))

df <- df %>%
  mutate(
    VEMF = factor(VEMF, levels = paste0("CM", 1:24))
  )

g1 <- df %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  ggplot(aes(x = Year, y = CP, group = VEMF, color = VEMF)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +  
  scale_x_discrete(
    breaks = c("2020",  "2040", "2060",  "2080","2100")
  ) +
  scale_y_continuous(name = expression(paste("Gro_rem_CO2 (Mt)")))

plot(g1)



name  <- paste0(thema, "ALL.png")

ggsave(
  filename = file.path(output_dir, name),
  plot = g1,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

#test