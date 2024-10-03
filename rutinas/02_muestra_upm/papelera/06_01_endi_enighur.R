
rm(list = ls())

{
  library(rio)
  library(tidyverse)
  library(janitor)
  library(readxl)
  library(rio)
}

final <- read_excel("muestra_upm_man_sec_fondo_rot_002.xlsx") %>% rename(periodo = "periodo_nuevo")
enighur_en_endi <- readRDS("D:/OMAR LLAMBO/enighur_2024/rutinas/02_muestra_upm/muestra_enighur_en_endi.rds")

                                                    
# cambio 1: 190650900201 (mi base per:11) por 190651900201 (prov - 1)
# cambio 2: 190158000101 (mi base per:6) por 190156900201 (prov - 2)


a_1 <- final %>% filter(id_upm %in% c("190650900201")) %>% select(id_upm,periodo)  %>% filter(!duplicated(id_upm))
a_2 <- final %>% filter(id_upm %in% c("190158000101")) %>% select(id_upm,periodo)  %>% filter(!duplicated(id_upm))

#--- UPM del estato 1422 que vamos a intercambiar
b_1 <- final %>% filter(id_upm %in% c("190651900201")) %>% select(id_upm,periodo) %>% filter(!duplicated(id_upm))
b_2 <- final %>% filter(id_upm %in% c("190156900201")) %>% select(id_upm,periodo)  %>% filter(!duplicated(id_upm))

#--- Generando el cambio
final <- final %>% mutate(
  periodo_nuevo = periodo,
  periodo_nuevo = ifelse(id_upm == a_1$id_upm, b_1$periodo, periodo_nuevo),
  periodo_nuevo = ifelse(id_upm == a_2$id_upm, b_2$periodo, periodo_nuevo),
  
  periodo_nuevo = ifelse(id_upm == b_1$id_upm, a_1$periodo, periodo_nuevo),
  periodo_nuevo = ifelse(id_upm == b_2$id_upm, a_2$periodo, periodo_nuevo))  

export(final,"muestra_upm_man_sec_fondo_rot_003.xlsx")


final <- read_excel("muestra_upm_man_sec_fondo_rot_003.xlsx") 
final_aux <- final %>% group_by(id_upm,periodo_nuevo) %>% summarise()
table(final_aux$periodo_nuevo)
 



