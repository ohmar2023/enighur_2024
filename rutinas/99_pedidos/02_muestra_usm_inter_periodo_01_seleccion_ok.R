

muestra_usm_inter <- import("productos/02_muestra_usm/periodo_01/muestra_usm_inter.rds") %>% 
  mutate(n_viv =  str_pad(n_viv, 4, "left", "0"))

dim(muestra_usm_inter)

muestra_per_1 <- import("productos/02_muestra_usm/periodo_01/muestra_periodo_01_myc.xlsx") %>% 
  mutate(aux = paste0(id_upm,zona,sector,manzana,num_edif,numviv))
  
muestra_usm_inter <- muestra_usm_inter %>% 
  mutate(aux = paste0(id_upm,zon,sec,man,n_umce,n_viv),
         aux_2 = ifelse(aux %in% muestra_per_1$aux, 1, 0), 
         aux_2 = ifelse(aux %in% c("1705500209010150010010080001","1705500209010150010010220001"),
                        1, aux_2)) %>% 
  left_join(select(muestra_per_1, no_orden, aux), by = "aux") %>% 
  mutate(n_aleatorio_orden = no_orden,
         n_aleatorio_orden = ifelse(aux == "1705500209010150010010080001",11,n_aleatorio_orden),
         n_aleatorio_orden = ifelse(aux == "1705500209010150010010220001",12,n_aleatorio_orden)) %>% 
  select(-c("aux","aux_2","no_orden"))

table(muestra_usm_inter$n_aleatorio_orden,  useNA = "ifany")

export(muestra_usm_inter, "muestra_usm_inter.rds")


