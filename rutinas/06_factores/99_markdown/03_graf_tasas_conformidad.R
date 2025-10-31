
# -----------------------------------------------------------------------------
# Tasas de conformidad a nivel de UPM
# -----------------------------------------------------------------------------


# Calculo de Tasas a nivel de UPM ---------------------------------------------

  tasas_conf_upm <- cobertura_base_total %>%
  group_by(id_upm, zonal, Elegibilidad) %>% 
  summarise(n = n()) %>%
  pivot_wider(names_from = Elegibilidad, values_from = n) %>%
  mutate_all(~replace(.,is.na(.),0)) %>%
  mutate( tre = re / (re + nr + ne + ed),
          tnr = nr / (re + nr + ne + ed),
          tne = ne / (re + nr + ne + ed),
          ted = ed / (re + nr + ne + ed)) %>% 
  mutate(inter_tre = case_when(tre >= 0.80 ~ "Mayor a 80%",
                               tre >= 0.50 & tre < 0.80 ~ "Entre 50% - 80%",
                               tre >= 0 & tre < 0.50 ~ "Menor a 50%", 
                               TRUE ~ "error")) %>% 
  mutate(id_conglomerado = substr(id_upm, 1,10), 
         n_rvo_aux = ifelse(tre >= 0.80, "Efectiva", "No efectiva"))


# Calculo de tasas de conf UPM por zonal ---------------------------------------

  tabla_tasas_conf_upm_zonal <- tasas_conf_upm %>%  
  group_by(Grupo = inter_tre, zonal) %>% 
  summarize(n = n()) %>% 
  group_by(zonal) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(porcentaje = 100 * round( n / total, 4))
  
  tabla_tasas_conf_upm_zonal[is.na(tabla_tasas_conf_upm_zonal)] <- 0

# Grafico de tasas de conf UPM por zonal ---------------------------------------
  
  graf_tasas_conf_upm_zonal <- ggplot(tabla_tasas_conf_upm_zonal) + 
    geom_bar(
      aes(x = zonal, y = n, fill = Grupo, group = Grupo), 
      stat='identity', position = 'dodge'
    ) +
    geom_text(
      aes(x = zonal, y = n, label = n, group = Grupo),
      position = position_dodge(width = 1),
      vjust = -0.5, size = 3.5
    ) + 
    theme_bw() +
    scale_fill_manual(values = c("paleturquoise2","#90EE90","#FA8072"))+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          axis.text.y = element_blank())+
    theme(legend.position = "bottom")
  

  
  # graf_tasas_conf_upm_zonal <- tabla_tasas_conf_upm_zonal  %>% 
  #  ggplot(aes(x= zonal, y = n , fill= TRE)) + 
  #  geom_col() + coord_flip() + 
  #  #labs(title= '', y= "Cantidad") +
  #  scale_y_continuous() +
  #  scale_fill_manual(name = "Tasas", 
  #                    labels = c(unique(tabla_tasas_conf_upm_zonal$TRE)), 
  #                    values = c("Entre 50% - 75%" = "#FAD211", 
  #                               "Mayor a 75%" = "#8BB657", 
  #                               "Menor a 50%" = "#D2241F"))+
  #   #geom_text(aes(label=paste0(n," ", "", "(", porcentaje, "%",")")),    #18
  #   geom_text(aes(label=n), 
  #            vjust=-0.5, 
  #             color="black", 
  #             hjust= -0.1,
  #             position = position_dodge(4.0),  
  #             angle=0, 
  #             size=4.0)
  # 
  # graf_tasas_conf_upm_zonal
 
  # tabla_tasas_upm_zonal %>% filter(TRE == "Menor a 50%") %>% 
  #  ggplot(aes(x= reorder(zonal, n), y= n)) + 
  #  geom_segment( aes(xend = zonal, y=0, yend = n), color= "gray", size= 1) + 
  #  geom_point( size=5, color= "steelblue") + 
  #  coord_flip() + 
  #  labs(title= 'Cantidad de Transacciones por Sucursal', y= "Cantidad", 
  #       x= "Sucursal", subtitle = "Tipo lollipop", caption = "HOLA") 

