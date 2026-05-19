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
setwd("0513")


files <- list(
  list(file = "JPN_SSP2i_CM1_NoCC_No.gdx", scenario = "CM1"),
  list(file = "JPN_SSP2i_CM2_NoCC_No.gdx", scenario = "CM2"),
  list(file = "JPN_SSP2i_CM3_NoCC_No.gdx", scenario = "CM3"),
  list(file = "JPN_SSP2i_CM4_NoCC_No.gdx", scenario = "CM4"),
  list(file = "JPN_SSP2i_CM5_NoCC_No.gdx", scenario = "CM5"),
  list(file = "JPN_SSP2i_CM6_NoCC_No.gdx", scenario = "CM6")
)


files <- list(
  list(file = "JPN_SSP2i_CM13_NoCC_No.gdx", scenario = "CM13"),
  list(file = "JPN_SSP2i_CM14_NoCC_No.gdx", scenario = "CM14"),
  list(file = "JPN_SSP2i_CM15_NoCC_No.gdx", scenario = "CM15"),
  list(file = "JPN_SSP2i_CM16_NoCC_No.gdx", scenario = "CM16"),
  list(file = "JPN_SSP2i_CM17_NoCC_No.gdx", scenario = "CM17"),
  list(file = "JPN_SSP2i_CM18_NoCC_No.gdx", scenario = "CM18")
)


files <- list(
  list(file = "JPN_SSP2i_CM19_NoCC_No.gdx", scenario = "CM19"),
  list(file = "JPN_SSP2i_CM20_NoCC_No.gdx", scenario = "CM20"),
  list(file = "JPN_SSP2i_CM21_NoCC_No.gdx", scenario = "CM21"),
  list(file = "JPN_SSP2i_CM22_NoCC_No.gdx", scenario = "CM22"),
  list(file = "JPN_SSP2i_CM23_NoCC_No.gdx", scenario = "CM23"),
  list(file = "JPN_SSP2i_CM24_NoCC_No.gdx", scenario = "CM24")
)

cms <- c(1:24)

files <- lapply(cms, function(i) {
  list(
    file = paste0("JPN_SSP2i_CM", i, "_NoCC_No.gdx"),
    scenario = paste0("CM", i)
  )
})
# COM_STO----------------------------------------------------------------

df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "QSTO2_load")
  df_temp <- gdx_data %>%
    rename(Year = "i") %>%
    rename(y_value = "value") %>%
    mutate(CCS = file_info$scenario) %>% 
    filter(k=="COM_STO")
  
  df <- rbind(df, df_temp)
}


df$CCS <- factor(df$CCS, levels = paste0("CM", cms))

g2 <- df %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  ggplot(aes(x = Year, y = y_value/100, group = CCS, color = CCS)) + 
  
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  #facet_wrap(~ j, scales = "free_y") + 
  scale_x_discrete(
    breaks = c( "2020","2040", "2060",  "2080",  "2100")
  ) +
  labs(y = "COM_STO (Mt/year)") +
  theme_1 +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

plot(g2)


name <- "COM_STO.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g2,
  units = "in",
  dpi = 300,
  bg = "white"
)


# PCCS --------------------------------------------------------------------



df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "PCCS_load")
  df_temp <- gdx_data %>%
    rename(Year = "i") %>%
    rename(y_value = "value") %>%
    mutate(SCENARIO = file_info$scenario) 
  
  df <- rbind(df, df_temp)
}

df$SCENARIO <- factor(df$SCENARIO, levels = paste0("CM", cms))


g2 <- df %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  ggplot(aes(x = Year, y = y_value, group = SCENARIO, color = SCENARIO)) + 
  
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  facet_wrap(~ j, scales = "free_y") + 
  scale_x_discrete(
    breaks = c( "2020","2040", "2060",  "2080",  "2100")
  ) +
  labs(y = "PCCS") +
  theme_1 +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

plot(g2)


name <- "PCCS.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g2,
  units = "in",
  dpi = 300,
  bg = "white"
)


# PGHG --------------------------------------------------------------------


df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "PGHG_load")
  df_temp <- gdx_data %>%
    rename(Year = "i") %>%
    rename(y_value = "value") %>%
    mutate(SCENARIO = file_info$scenario) 
  
  df <- rbind(df, df_temp)
}

df$SCENARIO <- factor(df$SCENARIO, levels = paste0("CM", cms))


g2 <- df %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  ggplot(aes(x = Year, y = y_value, group = SCENARIO, color = SCENARIO)) + 
  
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  facet_wrap(~ j, scales = "free_y") + 
  scale_x_discrete(
    breaks = c( "2020","2040", "2060",  "2080",  "2100")
  ) +
  labs(y = "PGHG") +
  theme_1 +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

plot(g2)


name <- "PGHG.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g2,
  units = "in",
  dpi = 300,
  bg = "white"
)


# EMALI --------------------------------------------------------------------


df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "EMALI_load")
  df_temp <- gdx_data %>%
    mutate(SCENARIO = file_info$scenario) %>% 
    filter(i4=="CO2") %>% 
    filter(i5=="10X")
  
  df <- rbind(df, df_temp)
}

df$SCENARIO <- factor(df$SCENARIO, levels = paste0("CM", cms))


g2 <- df %>% 
  filter(as.numeric(as.character(i1)) %% 5 == 0) %>%
  ggplot(aes(x = i1, y = value/100, group = i3, color = i3)) + 
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ SCENARIO, scales = "free_y") + 
  scale_x_discrete(
    breaks = c( "2020","2040", "2060",  "2080",  "2100")
  ) +
  labs(y = "CCS (Mt/year)") +
  theme_1 +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

plot(g2)


name <- "EMALI_10X.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g2,
  units = "in",
  dpi = 300,
  bg = "white"
)


# GHGT_IMP --------------------------------------------------------------------


df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "GHGT_IMP_load")
  df_temp <- gdx_data %>%
    mutate(SCENARIO = file_info$scenario) 
  
  df <- rbind(df, df_temp)
}

df$SCENARIO <- factor(df$SCENARIO, levels = paste0("CM", cms))


g2 <- df %>% 
  filter(as.numeric(as.character(i)) %% 5 == 0) %>%
  ggplot(aes(x = i, y = value/100, group = SCENARIO, color = SCENARIO)) + 
  
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_discrete(
    breaks = c( "2020","2040", "2060",  "2080",  "2100")
  ) +
  labs(y = "GHG import (Mt/year)") +
  theme_1 +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

plot(g2)


name <- "GHGT_IMP.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g2,
  units = "in",
  dpi = 300,
  bg = "white"
)

