
output_dir <- file.path("..", "output/laboseminar")

make_share_plot <- function(target_vec, target_label){
  
  CLP <- c("SSP2i_CM15_NoCC_No", "SSP2i_CM7_NoCC_No","SSP2i_CM24_NoCC_No")
  
  df <- rgdx.param("JPN_IAMC.gdx", "IAMC_template") %>%
    filter(VEMF %in% vec,
           SCENARIO %in% CLP) %>%
    mutate(
      VEMF = if_else(VEMF == target_vec, "Ele", "NonEle"),
      SCENARIO = recode(SCENARIO,
                        "SSP2i_CM7_NoCC_No" = "High",
                        "SSP2i_CM15_NoCC_No" = "Middle",
                        "SSP2i_CM24_NoCC_No" = "Low"),
      SCENARIO = factor(SCENARIO, levels = c("High", "Middle", "Low"))
    )
  
  df_share <- df %>%
    group_by(SCENARIO, YEMF, VEMF) %>%
    summarise(value = sum(IAMC_Template), .groups = "drop") %>%
    pivot_wider(names_from = VEMF,
                values_from = value,
                values_fill = 0) %>%   # ← 超重要（NA防止）
    mutate(share = Ele / (Ele + NonEle) * 100)
  
  ggplot(df_share,
         aes(x = YEMF,
             y = share,
             color = SCENARIO,
             group = SCENARIO)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    ylab(paste0(target_label, " share (%)")) +
    xlab("Year") +
    scale_x_discrete(breaks = c("2020","2040","2060","2080","2100")) +
    theme_1
}
vec <- c("Fin_Ene_Gas","Fin_Ene_Heat","Fin_Ene_Hyd",
         "Fin_Ene_Liq_Oil","Fin_Ene_Liq_Bio",
         "Fin_Ene_SolidsCoa","Fin_Ene_SolidsBio")

g1 <- make_share_plot("Fin_Ene_Hyd", "Hydrogen")
g2 <- make_share_plot("Fin_Ene_SolidsBio", "Bioenergy")

g <- (g1 + g2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.box = "horizontal")
plot(g)

name="Bio_Hyd.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

