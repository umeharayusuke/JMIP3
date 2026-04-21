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

output_dir <- file.path("..", "output/CCS")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# PCCS ----------------------------------------------------------------

files <- list(
  list(file = "JPN_SSP2i_CM1_NoCC_No.gdx", scenario = "1"),
  list(file = "JPN_SSP2i_CM2_NoCC_No.gdx", scenario = "2"),
  list(file = "JPN_SSP2i_CM3_NoCC_No.gdx", scenario = "3"),
  list(file = "JPN_SSP2i_CM4_NoCC_No.gdx", scenario = "4"),
  list(file = "JPN_SSP2i_CM5_NoCC_No.gdx", scenario = "5"),
  list(file = "JPN_SSP2i_CM6_NoCC_No.gdx", scenario = "6"),
  list(file = "JPN_SSP2i_CM7_NoCC_No.gdx", scenario = "7"),
  list(file = "JPN_SSP2i_CM8_NoCC_No.gdx", scenario = "8"),
  list(file = "JPN_SSP2i_CM9_NoCC_No.gdx", scenario = "9"),
  list(file = "JPN_SSP2i_CM10_NoCC_No.gdx", scenario = "10"),
  list(file = "JPN_SSP2i_CM11_NoCC_No.gdx", scenario = "11"),
  list(file = "JPN_SSP2i_CM12_NoCC_No.gdx", scenario = "12"),
  list(file = "JPN_SSP2i_CM13_NoCC_No.gdx", scenario = "13"),
  list(file = "JPN_SSP2i_CM14_NoCC_No.gdx", scenario = "14"),
  list(file = "JPN_SSP2i_CM15_NoCC_No.gdx", scenario = "15"),
  list(file = "JPN_SSP2i_CM16_NoCC_No.gdx", scenario = "16"),
  list(file = "JPN_SSP2i_CM17_NoCC_No.gdx", scenario = "17"),
  list(file = "JPN_SSP2i_CM18_NoCC_No.gdx", scenario = "18"),
  list(file = "JPN_SSP2i_CM19_NoCC_No.gdx", scenario = "19"),
  list(file = "JPN_SSP2i_CM20_NoCC_No.gdx", scenario = "20"),
  list(file = "JPN_SSP2i_CM21_NoCC_No.gdx", scenario = "21"),
  list(file = "JPN_SSP2i_CM22_NoCC_No.gdx", scenario = "22"),
  list(file = "JPN_SSP2i_CM23_NoCC_No.gdx", scenario = "23"),
  list(file = "JPN_SSP2i_CM24_NoCC_No.gdx", scenario = "24")
  
)

df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "PCCS_load")
  df_temp <- gdx_data %>%
    rename(Year = "i") %>%
    rename(y_value = "value") %>%
    #mutate(CCS = file_info$scenario)%>%
    mutate(
      CCS_num = as.numeric(file_info$scenario),
      CCS = paste0("CM", CCS_num),
      group = case_when(
        CCS_num <= 6  ~ "0%by2050",
        CCS_num <= 12 ~ "20%by2050",
        CCS_num <= 18 ~ "-5%by2050",
        TRUE          ~ "-20%by2050"
      )
    )
  
  df <- rbind(df, df_temp)
}

df$CCS <- factor(df$CCS, levels = paste0("CM", 1:24))

g2 <- df %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  
  ggplot(aes(x = Year, y = y_value*1000, group = CCS, color = group)) + 
  
  geom_line(linewidth = 1, alpha = 0.7) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  
  facet_wrap(~ group, ncol = 2, scales = "free_y") +
  
  scale_x_discrete(
    breaks = c("2040","2050", "2060","2070", "2080","2090", "2100")
  ) +
  
  scale_color_manual(values = c(
    "0%by2050"   = "#1b9e77",
    "20%by2050"  = "#d95f02",
    "-5%by2050" = "#7570b3",
    "-20%by2050" = "#e7298a"
  )) +
  
  labs(y = "PCCS (US$/tCO2)", color = "Group") +
  
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
  width = 10,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# COM_STO -----------------------------------------------------------------



df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "QSTO2_load")
  df_temp <- gdx_data %>%
    rename(Year = "i") %>%
    rename(y_value = "value") %>%
    filter(k=="COM_STO") %>% 
    #mutate(CCS = file_info$scenario)%>%
    mutate(
      CCS_num = as.numeric(file_info$scenario),
      CCS = paste0("CM", CCS_num),
      group = case_when(
        CCS_num <= 6  ~ "0%by2050",
        CCS_num <= 12 ~ "20%by2050",
        CCS_num <= 18 ~ "-5%by2050",
        TRUE          ~ "-20%by2050"
      )
    )
  df <- rbind(df, df_temp)
}

df$CCS <- factor(df$CCS, levels = paste0("CM", 1:24))

g2 <- df %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  
  ggplot(aes(x = Year, y = y_value*1000, group = CCS, color = group)) + 
  
  geom_line(linewidth = 1, alpha = 0.7) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  
  facet_wrap(~ group, ncol = 2, scales = "free_y") +
  
  scale_x_discrete(
    breaks = c("2040","2050", "2060","2070", "2080","2090", "2100")
  ) +
    scale_color_manual(values = c(
    "0%by2050"   = "#1b9e77",
    "20%by2050"  = "#d95f02",
    "-5%by2050" = "#7570b3",
    "-20%by2050" = "#e7298a"
  )) +
  
  labs(y = "COM_STO", color = "Group") +
  
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
  width = 10,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)



# QRED --------------------------------------------------------------------


df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "QRED_load")
  
  df_temp <- gdx_data %>%
    group_by(i1, CCS_type = i3) %>%
    summarise(total_value = sum(value), .groups = "drop") %>% 
    rename(Year = "i1") %>%
    mutate(
      CCS_num = as.numeric(file_info$scenario),
      CCS = paste0("CM", CCS_num),
      group = case_when(
        CCS_num <= 6  ~ "0%by2050",
        CCS_num <= 12 ~ "20%by2050",
        CCS_num <= 18 ~ "-5%by2050",
        TRUE          ~ "-20%by2050"
      )
    )
  
  df <- rbind(df, df_temp)
}

df$CCS <- factor(df$CCS, levels = paste0("CM", 1:24))


g2 <- df %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  
  ggplot(aes(x = Year, y = total_value*1000, group = CCS, color = group)) + 
  
  geom_line(linewidth = 1, alpha = 0.7) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  
  facet_grid( group~CCS_type, scales = "free_y") +
  
  scale_x_discrete(
    breaks = c("2040", "2060","2080", "2100")
  ) +
  
  scale_color_manual(values = c(
    "0%by2050"   = "#1b9e77",
    "20%by2050"  = "#d95f02",
    "-5%by2050" = "#7570b3",
    "-20%by2050" = "#e7298a"
  )) +
  
  labs(y = "QRED_load", color = "Group") +
  
  theme_1 +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

plot(g2)

name <- "QRED_load.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g2,
  width = 17,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

