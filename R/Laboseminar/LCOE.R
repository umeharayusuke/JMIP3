library(readxl)

df <- read_excel("LCOE.xlsx", sheet = "LCOE3")


df_long <- df %>%
  pivot_longer(
    cols = c(`2025`,`2050`,`2100`),
    names_to = "Year",
    values_to = "Value"
  )

range_df <- df_long %>%
  group_by(Ene) %>%
  summarise(
    min_val = min(Value),
    max_val = max(Value)
  )

order_vec <- df_long %>%
  filter(Year=="2100") %>%
  arrange(Value) %>%
  pull(Ene)

df_long$Ene <- factor(df_long$Ene, levels = order_vec)
range_df$Ene <- factor(range_df$Ene, levels = order_vec)


g<-ggplot() +
  geom_linerange(
    data = range_df,
    aes(x = Ene, ymin = min_val, ymax = max_val),
    linetype = "dashed",
    linewidth = 0.8,
    color = "grey40"
  ) +
  geom_point(
    data = df_long,
    aes(x = Ene, y = Value, color = Year, shape = Year),
    size = 3
  ) +
  theme_minimal(base_size = 14) +
  labs(
    x = "",
    y = "LCOE ($/kWh)",
    color = "Year",
    shape = "Year"
  ) +
  #scale_color_brewer(palette = "Dark2")+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot(g)


name="LCOE.png"
ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)