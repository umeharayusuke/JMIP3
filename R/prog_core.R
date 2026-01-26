df <- rgdx.param("JPN_IAMC2.gdx", "IAMC_template") %>%
  filter(VEMF %in% Fin_vec) %>%
  filter(YEMF == "2100") %>% 
  filter(SCENARIO %in% CLP)


df$SCENARIO <- factor(df$SCENARIO, levels = CLP)

g1 <- ggplot(data = df) +
  geom_bar(mapping = aes(x = SCENARIO, y = IAMC_Template, fill = VEMF), 
           stat = "identity", width = 0.7) +
  scale_fill_manual(
    values = Fin_col)+
  ylab("Primary energy \n in 2100 (EJ/yr)")+
  theme_1

plot(g1)
