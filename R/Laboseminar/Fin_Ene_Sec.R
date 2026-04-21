
theme_1 <- theme_bw() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, size = 14, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = "right")
output_dir <- file.path("..", "output/laboseminar")

CLP <- c("SSP2i_BaU_NoCC_No","SSP2i_CM15_NoCC_No",
         "SSP2i_CM7_NoCC_No","SSP2i_CM24_NoCC_No")

YEMF_target <- c("2020","2050","2100")

col_energy <- c(
  "Electricity" = "#FFD700",
  "Hydrogen"    = "#00BFFF",
  "Gases"       = "#1F78B4",
  "Liquids"     = "#A65628",
  "Solids"      = "#4D4D4D",
  "Other"       = "#BDBDBD"
)

rename_scenario <- function(df){
  df$SCENARIO <- recode(df$SCENARIO,
                        "SSP2i_BaU_NoCC_No"="BaU",
                        "SSP2i_CM7_NoCC_No"="High",
                        "SSP2i_CM15_NoCC_No"="Middle",
                        "SSP2i_CM24_NoCC_No"="Low")
  
  df$SCENARIO <- factor(df$SCENARIO,
                        levels=c("BaU","High","Middle","Low"))
  df
}

make_sector_plot <- function(vec, recode_fun, ylabel){
  
  df <- rgdx.param("JPN_IAMC.gdx","IAMC_template") %>%
    filter(VEMF %in% vec,
           YEMF %in% YEMF_target,
           SCENARIO %in% CLP) %>%
    mutate(VEMF = recode_fun(VEMF)) %>%
    rename_scenario()
  
  ggplot(df) +
    geom_bar(aes(SCENARIO, IAMC_Template, fill=VEMF),
             stat="identity", width=0.7) +
    scale_fill_manual(values=col_energy, drop=FALSE) +
    ylab(ylabel) +
    facet_wrap(~YEMF) +
    theme_1
}

# Industry

vec_ind <- c("Fin_Ene_Ind_Ele","Fin_Ene_Ind_Gas","Fin_Ene_Ind_Heat",
             "Fin_Ene_Ind_Hyd","Fin_Ene_Ind_Gas_Hyd_syn",
             "Fin_Ene_Ind_Liq","Fin_Ene_Ind_Oth","Fin_Ene_Ind_Solids")

recode_ind <- function(x){
  case_when(
    x=="Fin_Ene_Ind_Ele" ~ "Electricity",
    x=="Fin_Ene_Ind_Hyd" ~ "Hydrogen",
    grepl("Gas",x) ~ "Gases",
    x=="Fin_Ene_Ind_Liq" ~ "Liquids",
    x=="Fin_Ene_Ind_Solids" ~ "Solids",
    TRUE ~ "Other"
  )
}

g1 <- make_sector_plot(vec_ind, recode_ind,
                       "Final energy | Industry (EJ)")

# Building

vec_bld <- c("Fin_Ene_Res_and_Com_SolidsBio",
             "Fin_Ene_Res_and_Com_SolidsCoa",
             "Fin_Ene_Res_and_Com_Ele",
             "Fin_Ene_Res_and_Com_Gas",
             "Fin_Ene_Res_and_Com_Hyd",
             "Fin_Ene_Res_and_Com_Gas_Hyd_syn",
             "Fin_Ene_Res_and_Com_Liq")

recode_bld <- function(x){
  case_when(
    x=="Fin_Ene_Res_and_Com_Ele" ~ "Electricity",
    x=="Fin_Ene_Res_and_Com_Hyd" ~ "Hydrogen",
    grepl("Gas",x) ~ "Gases",
    grepl("Solids",x) ~ "Solids",
    x=="Fin_Ene_Res_and_Com_Liq" ~ "Liquids",
    TRUE ~ "Other"
  )
}

g2 <- make_sector_plot(vec_bld, recode_bld,
                       "Final energy | Building (EJ)")

# Transport

vec_tra <- c("Fin_Ene_Tra_Oth","Fin_Ene_Tra_Ele",
             "Fin_Ene_Tra_Gas","Fin_Ene_Tra_Hyd",
             "Fin_Ene_Tra_Liq")

recode_tra <- function(x){
  case_when(
    x=="Fin_Ene_Tra_Ele" ~ "Electricity",
    x=="Fin_Ene_Tra_Hyd" ~ "Hydrogen",
    x=="Fin_Ene_Tra_Gas" ~ "Gases",
    x=="Fin_Ene_Tra_Liq" ~ "Liquids",
    TRUE ~ "Other"
  )
}

g3 <- make_sector_plot(vec_tra, recode_tra,
                       "Final energy | Transportation (EJ)")



legend <- g_legend(
  g1 + guides(fill=guide_legend(nrow=1)) +
    theme(legend.position="bottom")
)

final_plot <-
  ((g1+theme(legend.position="none")) +
     (g2+theme(legend.position="none")) +
     (g3+theme(legend.position="none"))) /
  wrap_elements(legend) +
  plot_layout(heights=c(10,1))

plot(final_plot)

ggsave(file.path(output_dir,"Fin_Ene_Sec.png"),
       final_plot,
       width=12,height=6.5,dpi=300,bg="white")

