# -----------------------------------------------------------------------------
# Este script tiene como objetivo redistribuir la muestra a nivel de periodo
# para coordinal el enlistamiento con la ENDI. Las UPM que coinicden en la ENDI
# y la ENIGHUR son las que se redistribuirán a nivel de periodo. De estas UPM, 
# en la ENIGHUR se efectuará el cambio poniendolas desde el periodo 10 en 
# adelante, con esto se quiere garantizar que primero sean enlistadas en la ENDI
# para luego ser utilizadas en la ENIGHUR.
# -----------------------------------------------------------------------------

rm(list = ls())

{
  library(rio)
  library(tidyverse)
  library(janitor)
  library(readxl)
  library(rio)
}

# -----------------------------------------------------------------------------
# Lectura de bases de datos
# -----------------------------------------------------------------------------

#final <- read_excel("productos/02_muestra_upm/muestra_upm_man_sec_fondo_rot_002.xlsx") %>% rename(periodo = "periodo_nuevo")
final <- read_excel("muestra_upm_man_sec_fondo_rot_002_nueva.xlsx") %>% rename(periodo = "periodo_nuevo")
upm_fondo_rot <- read_excel("productos/02_muestra_upm/muestra_upm_periodo_fondo_rot.xlsx") 
enighur_en_endi <- readRDS("rutinas/02_muestra_upm/muestra_enighur_en_endi.rds") %>% 
  select(id_upm)

upm_mover <- enighur_en_endi %>% left_join(select(upm_fondo_rot, id_upm, periodo,fondo_rot_upm),
                              by = "id_upm") %>%  filter(periodo < 10) 

# -----------------------------------------------------------------------------
# Función para generar el cambio
# -----------------------------------------------------------------------------

cambiar_upm <- function(u_1,u_2,base)
{
  a_1 <- base %>% filter(id_upm %in% c(u_1)) %>% select(id_upm,periodo,semana)  %>% filter(!duplicated(id_upm))
  
  #--- UPM del estato 1422 que vamos a intercambiar
  b_1 <- base %>% filter(id_upm %in% c(u_2)) %>% select(id_upm,periodo,semana) %>% filter(!duplicated(id_upm))
  
  #--- Generando el cambio
  base <- base %>% mutate(
    periodo_nuevo = ifelse(id_upm == a_1$id_upm, b_1$periodo, periodo_nuevo),
    periodo_nuevo = ifelse(id_upm == b_1$id_upm, a_1$periodo, periodo_nuevo),
    
    semana_nueva =  ifelse(id_upm == a_1$id_upm, b_1$semana, semana_nueva),
    semana_nueva =  ifelse(id_upm == b_1$id_upm, a_1$semana, semana_nueva))
}

# -----------------------------------------------------------------------------
# Realizando los cambios: Se buscaron UPM en la misma parroquia, sino en 
# el mismo cantón o provincia, se debe ferificar que no sean UPM con fondo rot.
# -----------------------------------------------------------------------------

final <- final %>% mutate(periodo_nuevo = periodo)

final <- cambiar_upm("190151900101","190151000301",final) 
final <- cambiar_upm("190753000301","190750900301",final) 
final <- cambiar_upm("190151000101","190150000601",final) 
final <- cambiar_upm("190156900201","190158000401",final)
final <- cambiar_upm("190551900601","190551900701",final)
final <- cambiar_upm("190853000101","190850000401",final)
final <- cambiar_upm("190752900201","190550001301",final)
final <- cambiar_upm("210150016701","210150015601",final)
final <- cambiar_upm("170184012701","170184008501",final)
final <- cambiar_upm("190651900201","190650000801",final)

# -----------------------------------------------------------------------------
# Exportando la base resultante
# -----------------------------------------------------------------------------

export(final,"muestra_upm_man_sec_fondo_rot_003.xlsx")

# -----------------------------------------------------------------------------
# Controles
# -----------------------------------------------------------------------------

final %>% mutate(control = ifelse(periodo == periodo_nuevo,TRUE,FALSE)) %>% 
  filter(control == FALSE) %>% 
  filter(!duplicated(id_upm)) %>% 
  View("nuevo")

final %>% filter(!duplicated(id_upm)) %>% 
  group_by(zonal) %>% 
  summarise(n=n()) %>% 
  View("nuevo")

muestra_upm_man_sec %>% filter(!duplicated(id_upm)) %>% 
  group_by(zonal) %>% 
  summarise(n=n()) %>% 
  View("original")


uno <- c("190151900101",
         "190753000301",
         "190151000101",
         "190156900201",
         "190551900601",
         "190853000101",
         "190752900201",
         "210150016701",
         "170184012701",
         "190651900201")
 
dos <- c("190151000301",
         "190750900301",
         "190150000601",
         "190158000401",
         "190551900701",
         "190850000401",
         "190550001301",
         "210150015601",
         "170184008501",
         "190650000801")



