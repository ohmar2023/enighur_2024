aux <- read_excel("productos/02_muestra_upm/muestra_upm_man_sec_fondo_rot_003.xlsx") %>% 
  rename(periodo_003 = "periodo_nuevo",
         semana_003 = "semana") %>% filter(!duplicated(id_upm))


final %>% left_join(select(aux,id_upm,semana_003,periodo_003),by="id_upm") %>% 
  mutate(control_semana = ifelse(semana_nueva==semana_003,TRUE,FALSE),
         control_periodo = ifelse(periodo_nuevo==periodo_003,TRUE,FALSE)) %>% 
  View() 

table(final %>% filter(!duplicated(id_upm)) %>% select(semana_nueva))
table(final %>% filter(!duplicated(id_upm)) %>% select(periodo_nuevo))

final %>% group_by(periodo_nuevo) %>%
  mutate(mini = min(semana_nueva),
         maxi = max(semana_nueva)) %>% 
  ungroup() %>% 
  group_by(periodo_nuevo,mini,maxi) %>% 
  summarise() %>% View()


aux %>% group_by(periodo_003) %>%
  mutate(mini = min(semana_003),
         maxi = max(semana_003)) %>% 
  ungroup() %>% 
  group_by(periodo_003,mini,maxi) %>% 
  summarise() %>% View()


muestra_upm_man_sec <- readRDS("productos/02_muestra_upm/002/muestra_upm_man_sec.rds") 

final <- read_excel("muestra_upm_man_sec_fondo_rot_003_nueva.xlsx") 

final %>% left_join(select(muestra_upm_man_sec,id_upm,semana,periodo,man_sec),by = "man_sec") %>% 
  mutate(control_semana = ifelse(semana_nueva==semana,TRUE,FALSE),
         control_periodo = ifelse(periodo_nuevo==periodo,TRUE,FALSE)) %>% 
         View()

