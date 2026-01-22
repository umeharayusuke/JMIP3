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
setwd("data")

df0 <- read_csv("JPN_IAMC.csv")

df<- df0 %>% 
  filter(!SCENARIO %in% c("SSP2_BaU_NoCC_No", "SSP2_CM_NoCC_No", "SSP2i_CM_NoCC_No")) %>% 
  select(-`2055`,-`2065`, -`2075`, -`2085`, -`2095`)%>%
  mutate(SCENARIO = recode(
    SCENARIO,
    "SSP2i_BaU_NoCC_No"      = "JMIP_3_1__BASELINE",
    "SSP2i_CM1_NoCC_No"   = "JMIP_3_1__0by2050_20by2100",
    "SSP2i_CM2_NoCC_No"   = "JMIP_3_1__0by2050_0by2100",
    "SSP2i_CM3_NoCC_No"   = "JMIP_3_1__0by2050_-5by2100",
    "SSP2i_CM4_NoCC_No"   = "JMIP_3_1__0by2050_-20by2100",
    "SSP2i_CM5_NoCC_No"   = "JMIP_3_1__0by2050_-50by2100",
    "SSP2i_CM6_NoCC_No"   = "JMIP_3_1__0by2050_-100by2100",
    "SSP2i_CM7_NoCC_No"   = "JMIP_3_1__20by2050_20by2100",
    "SSP2i_CM8_NoCC_No"   = "JMIP_3_1__20by2050_0by2100",
    "SSP2i_CM9_NoCC_No"   = "JMIP_3_1__20by2050_-5by2100",
    "SSP2i_CM10_NoCC_No"   = "JMIP_3_1__20by2050_-20by2100",
    "SSP2i_CM11_NoCC_No"   = "JMIP_3_1__20by2050_-50by2100",
    "SSP2i_CM12_NoCC_No"   = "JMIP_3_1__20by2050_-100by2100",
    "SSP2i_CM13_NoCC_No"   = "JMIP_3_1__-5by2050_20by2100",
    "SSP2i_CM14_NoCC_No"   = "JMIP_3_1__-5by2050_0by2100",
    "SSP2i_CM15_NoCC_No"   = "JMIP_3_1__-5by2050_-5by2100",
    "SSP2i_CM16_NoCC_No"   = "JMIP_3_1__-5by2050_-20by2100",
    "SSP2i_CM17_NoCC_No"   = "JMIP_3_1__-5by2050_-50by2100",
    "SSP2i_CM18_NoCC_No"   = "JMIP_3_1__-5by2050_-100by2100",
    "SSP2i_CM19_NoCC_No"   = "JMIP_3_1__-20by2050_20by2100",
    "SSP2i_CM20_NoCC_No"   = "JMIP_3_1__-20by2050_0by2100",
    "SSP2i_CM21_NoCC_No"   = "JMIP_3_1__-20by2050_-5by2100",
    "SSP2i_CM22_NoCC_No"   = "JMIP_3_1__-20by2050_-20by2100",
    "SSP2i_CM23_NoCC_No"   = "JMIP_3_1__-20by2050_-50by2100",
    "SSP2i_CM24_NoCC_No"   = "JMIP_3_1__-20by2050_-100by2100",
  ))%>% 
  mutate(MODEL = "AIM/Hub-Japan 2.4")

write.csv(df, "JPN_IAMC_fix.csv", row.names = FALSE)


df_ref <- read_excel(
  "Updated JMIP data template 20251219.xlsx",
  sheet = "new_definitions 20251219"
)

vars_ref <- df_ref$Variable


# df_ref にはあるが df に無い VARIABLE
#すなわち今回の要求変数の提出漏れ変数リスト
df_nonmatch<-setdiff(vars_ref, unique(df$VARIABLE))
write.csv(df_nonmatch, "required_variable_but_not_in_AIMHub.csv", row.names = FALSE)


