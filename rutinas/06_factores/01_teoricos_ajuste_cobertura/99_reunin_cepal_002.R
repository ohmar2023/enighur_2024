

# Número de viviendas estimadas con el factor ajustado por no respuesta
sum(control$tre_3, na.rm = T) +
  sum(control$tne_2)

# expansion de personas con los diferentes factores
sum(wk2$totper * wk2$d3)
sum(wk2$totper * wk2$d0)


control_personas <- wk2 %>%  
  group_by(pro) %>% 
  mutate(personas = totper * d3) %>% 
  summarise(personas = sum(personas)) 


ylim <- c(0, 1.2*max(control_personas$personas))

barplot(control_personas$personas , border=F , names.arg=control_personas$pro , 
        las = 2 , 
        col = c("darkgreen", "bisque", "darkorange",  "darkorange") , 
        ylim = ylim , 
        main = "Diagrama de barras"
)

