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
library(readxl)

theme_1 <- theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, size = 16, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = "right", 
        #legend.title = element_blank(),
        strip.background = element_blank())
setwd("analysis")

df0 <- read_csv("Book1.csv")
plot(df0$CO2_cum, df0$Ele_rat)
fit <- lm(Ele_rat ~ CO2_cum, data=df0)
summary(fit)
abline(fit)
ggplot(df0, aes(CO2_cum, Ele_rat)) +
  geom_point(size=3) +
  geom_smooth(method="lm", se=TRUE)
fit2 <- lm(Ele_rat ~ CO2_cum + I(CO2_cum^2), data=df0)
summary(fit2)


# multiple ----------------------------------------------------------------
df0 <- read_csv("Book1.csv")

cor(df0)
m1 <- lm(Ele_rat ~ CO2_cum, data=df0)
summary(m1)
m2 <- lm(Ele_rat ~ Renew, data=df0)
summary(m2)
m3 <- lm(Ele_rat ~ DACCS, data=df0)
summary(m3)
full <- lm(Ele_rat ~ CO2_cum + Renew + DACCS, data=df0)
summary(full)
library(car)
vif(full)
step(full)
library(effectsize)
standardize_parameters(full)
library(car)
avPlots(full)

