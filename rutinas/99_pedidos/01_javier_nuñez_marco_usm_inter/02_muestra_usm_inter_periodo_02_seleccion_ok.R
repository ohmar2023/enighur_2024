
periodo <- 2
periodo <- str_pad(periodo, 2, "left", "0")
ruta <- paste0("productos/02_muestra_usm/","periodo_", periodo,"/muestra_usm_inter.rds")
muestra_usm_inter <- import(ruta) %>% 
  mutate(aux = paste0(id_upm,zon,sec,man,n_umce,n_viv))

muestra_per_2 <- import("productos/02_muestra_usm/periodo_02/muestra_usm_myc.xlsx") %>% 
  mutate(aux = paste0(id_upm,zona,sector,manzana,num_edif,numviv))

muestra_usm_inter <- muestra_usm_inter %>% 
  left_join(select(muestra_per_2, no_orden, aux), by = "aux") %>% 
  mutate(n_aleatorio_orden = no_orden,
         n_aleatorio  = pik) %>% 
  select(-freq, -n, -sel, -aux, -no_orden) %>% 
  select(names(aux))


export(muestra_usm_inter, "muestra_usm_inter.rds")
