
amb <- muestra_upm_man_sec %>% 
  #left_join(select(muestra,estrato,id_upm),by = "id_upm") %>% 
  mutate(cambio_sec = ifelse(man_sec %in% fondo_rotativo$muestra &
                               periodo %in% c(1,2,3) &
                               substr(man_sec,1,2)!="20",TRUE,FALSE)) %>% 
  filter(cambio_sec == TRUE)

fondo_rotativo %>% 
  filter( mes <= 3 &
    !fondo_rotativo$muestra %in% amb$man_sec) %>% View("no estan") 

final %>% filter()


muestra %>% group_by(estrato) %>% summarise(n()) %>% View() 

aux %>% filter(disp_upm==TRUE) %>% 
  group_by(estrato) %>% 
  summarise(n()) %>% View

aux %>% filter(estrato == "1421" & disp_upm == TRUE) %>% 
  group_by(id_upm) %>% 
  summarise(n()) %>% View


muestra_upm_man_sec %>% filter(id_upm %in%  c("140953901301","140954900301")) %>% View()









