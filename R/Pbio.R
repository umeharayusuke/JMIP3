#biopotential

df1 <- rgdx.param("JPN_SSP2i_CM24_NoCC_No.gdx", "Pwood_rsd") %>%
  filter(k == "kton") %>%
  mutate(
    value = as.numeric(value) *10*0.5,
    type = "Pwood_rsd"
  )

df2 <- rgdx.param("JPN_SSP2i_CM24_NoCC_No.gdx", "Pcrp_rsdt") %>%
  filter(k == "kton") %>%
  mutate(
    value = as.numeric(value) *10*0.5,
    type = "Pcrp_rsdt")

df <- bind_rows(df1, df2)

g <- ggplot(df, aes(x = i, y = value, fill = type)) +
  geom_col() +
  labs(
    x = "Year",
    y = "Pbiopotential (kton)",
    fill = ""
  ) +
  scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))+
  theme_minimal()


plot(g)


name  <- "biopotential.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
