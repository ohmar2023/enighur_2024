
ggplot(tabla_tasas_conf_upm_zonal) + 
  geom_bar(
    aes(x = zonal, y = n, fill = TRE, group = TRE), 
    stat='identity', position = 'dodge'
  ) +
  geom_text(
    aes(x = zonal, y = n, label = n, group = TRE),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 3.5
  ) + 
  theme_bw()

# Horizontal ------------------------------------------------------------------

ggplot(tabla_tasas_conf_upm_zonal) + 
  geom_bar(
    aes(x = zonal, y = n, fill = TRE, group = TRE), 
    stat='identity', position = 'dodge'
  ) +
  geom_text(
    aes(x = zonal, y = n, label = n, vjust = 3.5), 
    hjust = -0.5, size = 2,
    inherit.aes = TRUE
  ) + 
  coord_flip() + 
  theme_bw() 


names(tabla_tasas_conf_upm_zonal)


data = data_frame(
  week = as.factor(rep(c(1, 2), times = 5)),
  name = as.factor(rep(LETTERS[1:5], times = 2)),
  count = rpois(n = 10, lambda = 20),
  hjust = if_else(week == 1, 5, -5),
  vjust = if_else(week == 1, 3.5, -3.5)
)
