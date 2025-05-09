# -----------------------------------------------------------------------------
# Se identifican las UPM que tienen fondo rotativo y a su vez se procura
# intercambiarlas entre periodos, para que aquellas UPM que tengan fondos
# rotativos sean visitadas en los periodos posteriores al 4. El intercambio 
# se realizará entre UPM que esten en el mismo estrato, en el caso de que no 
# se pueda, se usará el siguiente estrato para efectuar el proceso. 
# La base y/o listado referente a los fondos rotativos está a nivel de man_sec
# por lo que, esta información se debe "pasar" a nivel de UPM, para lo cual 
# basta que un man_sec este en el listado para catalogar a la UPM con fondo rot.
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
# Lectura muestra
# -----------------------------------------------------------------------------

muestra <- readRDS("insumos/02_muestra_upm/muestra.rds")

# -----------------------------------------------------------------------------
# Lectura muestra espacio y tiempo
# -----------------------------------------------------------------------------

muestra_upm_man_sec <- readRDS("productos/02_muestra_upm/002/muestra_upm_man_sec.rds")
muestra_upm_periodo_semana <- readRDS("productos/02_muestra_upm/002/muestra_upm_periodo_semana.rds")

# -----------------------------------------------------------------------------
# Lectura base de fondos rotativos
# -----------------------------------------------------------------------------

fondo_rotativo <- read_excel("insumos/02_muestra_upm/fondos_rotativos/FONDO ROTATIVO Formato ENIGHUR (002)_nacional.xlsx", 
                             sheet = "DETALLE SEC_MAN") %>% clean_names()
  
aux <- muestra_upm_man_sec %>% 
  left_join(select(muestra,estrato,id_upm),by = "id_upm") %>% 
  mutate(cambio_sec = ifelse(man_sec %in% fondo_rotativo$muestra &
           periodo %in% c(1,2,3,4) &
           substr(man_sec,1,2)!="20",TRUE,FALSE)) %>% 
  group_by(id_upm) %>% 
  mutate(cambio_upm = ifelse(sum(cambio_sec) > 0,TRUE,FALSE),
         cambio_sec = ifelse(cambio_upm == TRUE,TRUE,cambio_sec),
         aux_cambio = n_distinct(man_sec),
         super_man = substr(id_upm,11,12),
         disp_upm = ifelse(cambio_upm == FALSE & 
                             sum(man_sec %in% fondo_rotativo$muestra) == 0 &
                              super_man == "01" & 
                              periodo > 4 ,TRUE,FALSE)) %>% 
   ungroup() %>% 
  mutate(cambio_upm = ifelse(id_upm %in%  c("140953901301","140954900301"), FALSE, cambio_upm))


# -----------------------------------------------------------------------------
# Bases IDS x UPM
# -----------------------------------------------------------------------------

id_upm_cambio <- aux %>% filter(cambio_upm == 1) %>%
  group_by(id_upm,estrato,periodo,semana) %>% 
  summarise() %>% 
  ungroup() %>% 
  arrange(estrato)

estrato_cambio <- id_upm_cambio %>% 
  group_by(estrato) %>% 
  summarise(n = n()) %>% 
  ungroup()

set.seed(19092024)

id_upm_disponible <- aux %>% filter(disp_upm == TRUE) %>% 
  group_by(id_upm,estrato,periodo,semana) %>% 
  summarise() %>% 
  ungroup() %>% 
  left_join(estrato_cambio, by = "estrato") %>% 
  filter(!is.na(n)) %>% 
  group_by(estrato) %>% 
  sample_n(unique(n)) %>% 
  arrange(estrato) #%>% group_by(estrato) %>% summarise(n())
  
# -----------------------------------------------------------------------------
# Periodos nuevos
# -----------------------------------------------------------------------------

a <- cbind(id_upm_cambio,
      periodo_nuevo = id_upm_disponible$periodo,
      semana_nueva = id_upm_disponible$semana)

b <- cbind(id_upm_disponible %>% select(-n),
      periodo_nuevo = id_upm_cambio$periodo,
      semana_nueva = id_upm_cambio$semana)

a_b <- rbind(a,b)

# -----------------------------------------------------------------------------
# Periodos nuevos
# -----------------------------------------------------------------------------

final <- muestra_upm_man_sec %>% 
  left_join(select(a_b, semana_nueva,periodo_nuevo,id_upm,estrato),by = c("id_upm")) %>% 
  mutate(periodo_nuevo = ifelse(is.na(periodo_nuevo),periodo,periodo_nuevo),
         semana_nueva = ifelse(is.na(semana_nueva),semana,semana_nueva),
         control = periodo == periodo_nuevo) 

# -----------------------------------------------------------------------------
# Controles
# -----------------------------------------------------------------------------

final %>% 
  mutate(control = periodo == periodo_nuevo) %>% View()

# --- upm faltante
aux_2 <- final %>% filter(control == FALSE)
aux_2 %>% filter(id_upm %in% a$id_upm)

# --- Control de upm cambiadas
aux %>% filter(cambio_upm == TRUE) %>% group_by(id_upm) %>% summarise() %>% dim()
final %>% filter(control == FALSE) %>% group_by(id_upm) %>% summarise() %>% dim()

# --- Control por periodo y por semana
final %>% group_by(semana) %>% summarise(n()) %>% view()
muestra_upm_man_sec %>% group_by(semana) %>% summarise(n()) %>% view()

# --- Deben haber UPM 23 UPM que intercambiar, por lo que seruan 46

final %>% filter(control == FALSE) %>% filter(!duplicated(id_upm)) %>% dim()
aux %>% filter(cambio_upm == TRUE) %>% filter(!duplicated(id_upm)) %>% dim()

# ---- No deben haber UPM en los periodos 1,2,3,4 (SALVO 2 QUE SE CAMBIAN LUEGO)
final %>% 
  mutate(cambiada = ifelse(man_sec %in% fondo_rotativo$muestra,1,0)) %>% 
  filter(cambiada == 1) %>% 
  select(periodo_nuevo) %>% 
  table()


# -----------------------------------------------------------------------------
# Cambio de las 2 UPM faltantes, las vamos a cambiar con 2 de otros estratos
# -----------------------------------------------------------------------------

# #--- UPM del estato 1421 que aun no han sido cambiadas
# 
# a_1 <- final %>% filter(id_upm %in% c("140953901301")) %>% select(id_upm,periodo,semana)  %>% filter(!duplicated(id_upm))
# a_2 <- final %>% filter(id_upm %in% c("140954900301")) %>% select(id_upm,periodo,semana)  %>% filter(!duplicated(id_upm))
# 
# 
# #--- UPM del estato 1422 que vamos a intercambiar
# b_1 <- final %>% filter(id_upm %in% c("141251000201")) %>% select(id_upm,periodo,semana) %>% filter(!duplicated(id_upm))
# b_2 <- final %>% filter(id_upm %in% c("140952000101")) %>% select(id_upm,periodo,semana)  %>% filter(!duplicated(id_upm))
# 
# #--- Generando el cambio
# final <- final %>% mutate(
#   periodo_nuevo = ifelse(id_upm == a_1$id_upm, b_1$periodo, periodo_nuevo),
#   periodo_nuevo = ifelse(id_upm == a_2$id_upm, b_2$periodo, periodo_nuevo),
#   
#   periodo_nuevo = ifelse(id_upm == b_1$id_upm, a_1$periodo, periodo_nuevo),
#   periodo_nuevo = ifelse(id_upm == b_2$id_upm, a_2$periodo, periodo_nuevo))  
  
# -----------------------------------------------------------------------------
# FUNCIÓN PARA REALIZAR EL CAMBIO
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

final <- cambiar_upm("140953901301","141251000201",final) 
final <- cambiar_upm("140954900301","140952000101",final) 
final <- final %>% mutate(control = periodo == periodo_nuevo)

# -----------------------------------------------------------------------------
# Exportando
# -----------------------------------------------------------------------------

export(final,"muestra_upm_man_sec_fondo_rot_002_nueva.xlsx")

