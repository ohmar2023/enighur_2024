
v_pro_auto <- c("17", #Quito
                "09", #Guayaquil
                "01",#Cuenca
                "07", #Machala
                "18", #Ambato
                "08", #Esmeraldas
                "23", #Santo Domingo
                "13", #Manta
                "11") #Loja



cobertura_base_total_ciu_aux <- cobertura_base_total %>% 
  mutate(ciu_auto = paste0(pro, can, par)) %>% 
  filter(! ciu_auto %in% v_ciu_auto, 
           pro %in% v_pro_auto)

a=cobertura_base_total_ciu_aux %>% 
  filter(Elegibilidad != "error") %>% 
  arrange(desc(Elegibilidad)) %>% 
  ggplot(aes(y = pro, fill = Elegibilidad)) +
  geom_bar(position = "fill", alpha = 1.25, width = 0.9) + 
  geom_vline(linetype = "dotted", xintercept = 0.8, color = "black", size = 0.8) + 
  scale_x_continuous(breaks = c(0, 0.1, 0.2 ,0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
                     labels = c("0" = "0%", "0.1" = "10%", "0.2" = "20%" ,"0.3"="30%", 
                                "0.4" = "40%", "0.5"="50%", "0.6"="60%", "0.7"="70%", 
                                "0.8" = "80%", "0.9"="90%", "1.0"="100%"))  +
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.background = element_rect(fill="white"),
        axis.text = element_text(size=7),
        axis.title = element_text(size=9),
        axis.title.x = element_blank(),
        legend.box = "vertical",
        legend.box.spacing = unit(0.5, "cm"),
        legend.direction = "vertical", 
        legend.position = "right",
        legend.text = element_text(size=7),
        legend.title = element_text(size=8),
        legend.title.align = 0.5,
        panel.border = element_rect(colour = "black", fill = NA))  +
  scale_fill_manual(values = c("#FAD211","orange2", "#D2241F", "#8BB657" )) + 
  scale_color_manual(values = c("#FAD211","orange2", "#D2241F", "#8BB657" ))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())


elegibilidad_por_resto_prov <- cobertura_base_total_ciu_aux %>% 
  filter(Elegibilidad != "error") %>% 
  group_by(pro, Elegibilidad) %>% 
  summarise(n = n()) %>%
  group_by(pro) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(porcentaje = round(100 * n / total, 2)) %>% 
  pivot_wider(id_cols = c(-n, -total),
              names_from = Elegibilidad, 
              values_from = porcentaje)

rio::export(elegibilidad_por_resto_prov, 
            "documentos/comisión manta/ok_elegibilidad_por_resto_prov.xlsx")


