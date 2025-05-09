
# UPM del estrato 1421 que tienen fondos rotativos
aux %>% filter(estrato == "1421", cambio_upm == TRUE) %>% 
  group_by(id_upm,periodo) %>%
  summarise() %>% 
  View("tienen fondos")
  
# UPM del estrato 1421 disponobles para el cambio
aux %>% filter(estrato == "1421", disp_upm == TRUE) %>% 
  group_by(id_upm,periodo) %>%
  summarise() %>% 
  View("disponibles")  

# UPM del estrato 1421 disponobles para el cambio
aux %>% filter(estrato == "1421") %>% 
  group_by(id_upm,periodo) %>%
  summarise() %>% 
  View("aux")  

# aux 

aux %>% filter(cambio_sec == TRUE)  %>% dim()
final %>% filter(control == FALSE) %>% group_by(id_upm) %>% summarise() %>% dim()

muestra_upm_man_sec %>% filter(id_upm %in%  c("140953901301","140954900301")) %>% View()


# control base 001 enviada

b_001 <- read_xlsx("muestra_upm_man_sec_fondo_rot_001.xlsx")

final %>% left_join(select(b_001,man_sec,per_001=periodo_nuevo), by = "man_sec") %>% 
  mutate(diff = ifelse(per_001==periodo_nuevo,TRUE,FALSE)) %>% 
View()







