
# -----------------------------------------------------------------------------
# Tasas de conformidad 
# -----------------------------------------------------------------------------


# Calculo de Tasas a nivel de UPM ---------------------------------------------

conformidad_upm <- cobertura_base_total %>%
  group_by(id_upm, Elegibilidad) %>% 
  summarise(n = n()) %>%
  pivot_wider(names_from = Elegibilidad, values_from = n) %>%
  mutate_all(~replace(.,is.na(.),0)) %>%
  mutate( tre = re / (re + nr + ne + ed),
          tnr = nr / (re + nr + ne + ed),
          tne = ne / (re + nr + ne + ed),
          ted = ed / (re + nr + ne + ed)) %>% 
  mutate(inter_tre = case_when(tre >= 0.75 ~ "Mayor a 75%",
                               tre >= 0.550 & tre < 0.75 ~ "Entre 50% - 75%",
                               tre >= 0 & tre < 50 ~ "Menor a 50%", 
                               TRUE ~ "error")) %>% 
  mutate(id_conglomerado = substr(id_upm, 1,10), 
         n_rvo_aux = ifelse(tre >= 0.75, "Efectiva", "No efectiva"))

# upm_geo %>% 
#   group_by(zonal, inter_tre) %>% 
#   summarise(n = n()) %>% 
#   st_drop_geometry() %>%
#   filter(!is.na(inter_tre)) %>% 
#   adorn_totals() %>% 
#   View()



# Tasas de conformidad por UPM  ------------------------------------------------

graf_tasas_upm <- ggplot(conformidad_upm %>%  
                           group_by(TRE = inter_tre) %>% 
                           summarize(n = n()) %>% 
                           arrange(TRE), 
                         aes(x = TRE, y = n, fill = TRE)) +
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("#FAD211", "#8BB657", "#D2241F" )) +
  scale_color_manual(values = c("#FAD211", "#D2241F", "#8BB657" ))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

