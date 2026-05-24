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
library(writexl)
library(openxlsx)


setwd("data")
setwd("0520")


df0 <- read_csv("JPN_IAMC.csv")

df <- df0 %>% 
  filter(!SCENARIO %in% c("SSP2_BaU_NoCC_No", "SSP2_CM_NoCC_No", "SSP2i_CM_NoCC_No")) %>% 
  select(-`2055`, -`2065`, -`2075`, -`2085`, -`2095`) %>%
  
  mutate(SCENARIO = recode(
    SCENARIO,
    "SSP2i_BaU_NoCC_No"    = "JMIP_3_1__BASELINE",
    "SSP2i_CM1_NoCC_No"    = "JMIP_3_1__0by2050_20by2100",
    "SSP2i_CM2_NoCC_No"    = "JMIP_3_1__0by2050_0by2100",
    "SSP2i_CM3_NoCC_No"    = "JMIP_3_1__0by2050_-5by2100",
    "SSP2i_CM4_NoCC_No"    = "JMIP_3_1__0by2050_-20by2100",
    "SSP2i_CM5_NoCC_No"    = "JMIP_3_1__0by2050_-50by2100",
    "SSP2i_CM6_NoCC_No"    = "JMIP_3_1__0by2050_-100by2100",
    "SSP2i_CM7_NoCC_No"    = "JMIP_3_1__20by2050_20by2100",
    "SSP2i_CM8_NoCC_No"    = "JMIP_3_1__20by2050_0by2100",
    "SSP2i_CM9_NoCC_No"    = "JMIP_3_1__20by2050_-5by2100",
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
    "SSP2i_CM24_NoCC_No"   = "JMIP_3_1__-20by2050_-100by2100"
  )) %>%
  
  mutate(MODEL = "AIM/Hub-Japan 2.4") %>% 
  mutate(REGION = "Japan") %>%
  
  mutate(VARIABLE = recode(
    VARIABLE,
    "Trade|Emissions Allowances [Volume]" = "Trade|Emission Allowance"
  )) %>%
  
  mutate(
    UNIT = recode(
      UNIT,
      "USD_2010/kW"            = "US$2010/kW",
      "USD_2010/kW/yr"            = "US$2010/kW/yr",
      "billion USD_2010/yr"    = "billion US$2010/yr",
      "billion tkm/yr"         = "bn tkm/yr",
      "billion pkm/yr"         = "bn pkm/yr",
      "billion USD_2017/yr"    = "billion US$2010/yr",
      "USD_2010/t CO2"         = "US$2010/t CO2",
      "USD_2010/GJ"            = "US$2010/GJ"
    ),
    UNIT = if_else(
      VARIABLE == "Trade|Emission Allowance" &
        UNIT == "Mt CO2-equiv/yr",
      "Mt CO2/yr",
      UNIT
    )
  )
# AIMHub result is import of emission, but submission required variable is net export of emission
df <- df %>%
  mutate(
    across(
      c(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, 
        `2045`, `2050`, `2060`, `2070`, `2080`, `2090`, `2100`),
      ~ if_else(VARIABLE == "Trade|Emission Allowance",
                as.numeric(.x) * -1,
                as.numeric(.x))
    )
  )

write.xlsx(df, "JPN_IAMC.xlsx", rowNames = FALSE)



df_ref <- read_excel(
  "Updated JMIP data template 20251219.xlsx",
  sheet = "new_definitions 20251219"
)

vars_ref <- df_ref$Variable


# df_ref にはあるが df に無い VARIABLE
#すなわち今回の要求変数の提出漏れ変数リスト
df_nonmatch<-setdiff(vars_ref, unique(df$VARIABLE))
write.csv(df_nonmatch, "Not_Submission.csv", row.names = FALSE)


