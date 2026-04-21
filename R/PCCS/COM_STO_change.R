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


# ----------------------------------------------------------------

files <- list(
  list(file = "JPN_SSP2i_CM6_NoCC_No_bf.gdx", scenario = "Before"),
  list(file = "JPN_SSP2i_CM6_NoCC_No.gdx", scenario = "After")
)
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



g2 <- df %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  ggplot(aes(x = Year, y = y_value, group = CCS, color = CCS)) + 
  
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  facet_wrap(~ j, scales = "free_y") + 
  scale_x_discrete(
    breaks = c( "2020","2040", "2060",  "2080",  "2100")
  ) +
  labs(y = "COM_STO") +
  theme_1 +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

plot(g2)


name <- "COM_STO_change.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g2,
  units = "in",
  dpi = 300,
  bg = "white"
)

