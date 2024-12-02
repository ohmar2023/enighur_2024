muestra_upm_estratro <- read_excel("muestra_upm_man_sec_fondo_rot_004.xlsx")


aux <- muestra_upm_estratro %>% 
  left_join(select(marco_upm,estrato,id_upm),by = "id_upm") %>% 
  select(id_upm,estrato,semana_nueva,periodo_nuevo) %>% 
  filter(!duplicated(id_upm))

export(aux,"muestra_estrato.xlsx")
