

aux <- c("100250007401",
         "100453900201",
         "170170005201", # NO LEVANTADA
         "170550002101",
         "170550027301",
         "230150098101",
         "230150075101",
         "020250900401",
         "230150074901",
         "230150902401",
         "230150034301",
         "230150117701",
         "230150075901")

base <- base %>% filter(id_upm %in% aux) 

unique(base$id_upm)

resumen_dica <- read_excel("insumos/03_enlistamiento/2024_12_13/Resumen UPMs y correspondencia P1 y P2.xlsx") %>% 
  clean_names() %>% 
  rename(id_upm = upm) %>% 
  filter(!duplicated(id_upm)) %>% 
  select(id_upm,estado_upm)


base %>% group_by(id_upm) %>% 
  summarise(n_enlista = n_distinct(man_sec_21),
            sem = unique(semana_nueva),
            per = unique(periodo_nuevo),
            estrato = unique(estrato)) %>% 
  left_join(muestra %>% filter(id_upm %in% aux) %>% 
              group_by(id_upm) %>% summarise(n_muestra = n_distinct(man_sec))) %>% 
  left_join(resumen_dica, by = "id_upm") %>% 
  View("cambiar")

xs <- muestra %>% filter(periodo_nuevo == 1,!duplicated(id_upm))
table(xs$semana_nueva)  

resumen_dica %>% group_by(id_upm) %>% 
  summarise(n_distinct(estado_upm)) %>% View()

base %>% filter()



