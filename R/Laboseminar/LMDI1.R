

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
        legend.position = "right")
setwd("analysis")
output_dir <- file.path("..", "output/laboseminar")

df <- read_csv("LMDI.csv")
df <- df %>% arrange(Year)


log_mean <- function(a,b){
  if(a == b) return(a)
  (a-b)/(log(a)-log(b))
}

run_glmdi <- function(base, target){
  
  gross0 <- base$Gro_Emi
  gross1 <- target$Gro_Emi
  
  rem0 <- base$Gro_Rem
  rem1 <- target$Gro_Rem
  
  factors <- c("GDP","EI","Ele","CI")
  
  f0 <- as.numeric(base[factors])
  f1 <- as.numeric(target[factors])
  
  names(f0) <- factors
  names(f1) <- factors
  
  Lg <- log_mean(gross1, gross0)
  effects <- Lg * log(f1/f0)
  
  Lr <- log_mean(abs(rem1), abs(rem0))
  rem_effect <- - Lr * log(abs(rem1)/abs(rem0))
  
  result <- c(effects, Removal = rem_effect)
  
  data.frame(
    Factor = names(result),
    Contribution = as.numeric(result)
  )
}

#-------------------------
# 各期間で実行
#-------------------------
res_2020_2050 <- run_glmdi(df[1,], df[2,]) %>%
  mutate(Period = "2020-2050")

res_2050_2100 <- run_glmdi(df[2,], df[3,]) %>%
  mutate(Period = "2050-2100")

plot_df <- bind_rows(res_2020_2050, res_2050_2100)

# 並び順固定（論文では重要）
plot_df$Factor <- factor(
  plot_df$Factor,
  levels = c("GDP","EI","Ele","CI","Removal")
)

#-------------------------
# 横並びプロット
#-------------------------
cols <- c(
  GDP = "#d73027",
  EI = "#4575b4",
  Ele = "#fdae61",
  CI = "#1a9850",
  Removal = "#542788"
)

g<-ggplot(plot_df, aes(x="Net Change", y=Contribution, fill=Factor)) +
  geom_bar(stat="identity", width=0.7) +
  facet_wrap(~Period, nrow=1) +
  scale_fill_manual(values=cols) +
  theme_minimal(base_size = 15) +
  labs(
    y="Contribution to CO2 change",
    x=""
  ) +
  theme(
    legend.position="bottom",
    strip.text = element_text(face="bold")
  )

plot(g)


name="LMDI1.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)