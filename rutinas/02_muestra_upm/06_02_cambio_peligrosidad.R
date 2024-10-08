
aux <- muestra_upm_man_sec %>% 
  left_join(select(muestra,estrato,id_upm),by = "id_upm") %>% 
  mutate(cambio_sec = ifelse(man_sec %in% fondo_rotativo$muestra &
                               periodo %in% c(1,2,3,4) &
                               substr(man_sec,1,2)!="20",TRUE,FALSE),
         congl = substr(id_upm,1,10)) %>%
  group_by(congl) %>% 
  mutate(
    super_man = ifelse(n_distinct(substr(id_upm,11,12)) > 1,TRUE,FALSE)) %>%
  ungroup() %>% 
  group_by(id_upm) %>% 
  mutate(cambio_upm = ifelse(sum(cambio_sec) > 0,TRUE,FALSE),
         cambio_sec = ifelse(cambio_upm == TRUE,TRUE,cambio_sec),
         aux_cambio = n_distinct(man_sec),
         disp_upm = ifelse(cambio_upm == FALSE & 
                             sum(man_sec %in% fondo_rotativo$muestra) == 0 &
                             super_man == FALSE & 
                             periodo > 4 ,TRUE,FALSE)) %>% 
  ungroup()
