
rm(list = ls())

{
  library(rio)
  library(tidyverse)
  library(janitor)
  library(readxl)
  library(rio)
  library(xlsx)
}

# -----------------------------------------------------------------------------
# Lectura de bases de datos
# -----------------------------------------------------------------------------

muestra_upm_man_sec <- readRDS("productos/02_muestra_upm/002/muestra_upm_man_sec.rds")

peligrosos <- read_excel("insumos/02_muestra_upm/peligrosos/sectores_peligrosos_DIES.xlsx") %>% 
  clean_names() %>% rename(muestra = conglomerado,
                           zonal = coordinacion_zonal) %>% 
  mutate(muestra = substr(muestra,1,12),
         muestra = str_pad(muestra,12,"left","0"))


muestra <- readRDS("insumos/02_muestra_upm/muestra.rds")

muestra_upm_man_sec_fondo_rot_003_nueva <- read_excel("muestra_upm_man_sec_fondo_rot_003_nueva.xlsx")

fondo_rotativo <- read_excel("insumos/02_muestra_upm/fondos_rotativos/FONDO ROTATIVO Formato ENIGHUR (002)_nacional.xlsx", 
                             sheet = "DETALLE SEC_MAN") %>% clean_names()

enighur_en_endi <- readRDS("rutinas/02_muestra_upm/muestra_enighur_en_endi.rds") %>% 
  select(id_upm)

#peligrosos$muestra %in% enighur_en_endi$id_upm %>% sum()

# -----------------------------------------------------------------------------
# Exploratorio
# -----------------------------------------------------------------------------

# --- Caracteres por UPM en pligrosos
table(nchar(peligrosos$muestra))

# --- upm peligrosas que aparecen en la muestra _ 19
muestra %>% filter(id_upm %in% peligrosos$muestra) %>% select(id_upm)

# --- upm que aparecen en la muestra y deben ser cambiadas (periodo < 7) : 10
muestra_upm_man_sec_fondo_rot_003_nueva %>% 
  filter(id_upm %in% peligrosos$muestra & periodo_nuevo < 7) %>% 
  filter(!duplicated(id_upm)) %>%
  dim()

# --- upm que aparecen en la muestra y NO deben ser cambiadas (periodo >= 7): 9
muestra_upm_man_sec_fondo_rot_003_nueva %>% 
  filter(id_upm %in% peligrosos$muestra & periodo_nuevo >= 7) %>% 
  filter(!duplicated(id_upm)) %>% 
  dim()

# -----------------------------------------------------------------------------
# Preparación de la base
# -----------------------------------------------------------------------------

base <- muestra_upm_man_sec_fondo_rot_003_nueva %>% 
  left_join(select(muestra_upm_man_sec,id_upm,man_sec,semana,
                   periodo),by = c("id_upm","man_sec")) %>% 
  mutate(cambio_previo = ifelse(periodo_nuevo != periodo,TRUE,FALSE)) %>% 
  select( "zonal", "id_upm", "man_sec", "pro", "can", "par", "zon", "sec", "man", "semana_nueva",
          "periodo_nuevo", "cambio_previo") %>% 
  rename(semana = semana_nueva, periodo = periodo_nuevo)

# -----------------------------------------------------------------------------
# Lectura de bases de datos
# -----------------------------------------------------------------------------

aux <- base %>% 
  left_join(select(muestra,estrato,id_upm),by = "id_upm") %>% 
  mutate(congl = substr(id_upm,1,10)) %>%
  group_by(congl) %>% 
  mutate(
    super_man = ifelse(n_distinct(substr(id_upm,11,12)) > 1,TRUE,FALSE)) %>%
  ungroup() %>% 
  group_by(id_upm) %>% 
  mutate(cambio_upm = ifelse(id_upm %in% peligrosos$muestra & periodo < 7,TRUE,FALSE),
         cambio_sec = ifelse(cambio_upm == TRUE,TRUE,FALSE),
         fondo_rot = ifelse(id_upm %in% fondo_rotativo$muestra,TRUE,FALSE),
         endi = ifelse(id_upm %in% enighur_en_endi$id_upm,TRUE,FALSE),
         disp_upm = ifelse(cambio_upm == FALSE & 
                             super_man == FALSE &
                             cambio_previo == FALSE & 
                             fondo_rot == FALSE &
                             endi == FALSE &
                             !id_upm %in% peligrosos$muestra &
                             periodo > 7 ,TRUE,FALSE)) %>% 
  ungroup()

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

set.seed(09102024)

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

final <- base %>% 
  left_join(select(a_b, semana_nueva,periodo_nuevo,id_upm,estrato),by = c("id_upm")) %>% 
  mutate(periodo_nuevo = ifelse(is.na(periodo_nuevo),periodo,periodo_nuevo),
         semana_nueva = ifelse(is.na(semana_nueva),semana,semana_nueva),
         control = periodo == periodo_nuevo) 

# -----------------------------------------------------------------------------
# Control final
# -----------------------------------------------------------------------------

control_final <- final %>% filter(!duplicated(id_upm))
control_anterior <- muestra_upm_man_sec %>% filter(!duplicated(id_upm))

# --- Cuantas UPM fueron modificadas : 10 + 10 = 20
final %>% filter(control == FALSE, !duplicated(id_upm)) %>% dim()
# --- Debe sumar 13 : mismo tamaño en cada periodo
sum(table(control_final$periodo_nuevo) == table(control_anterior$periodo))
# --- Debe sumar 52 : mismo tamaño en cada semana
sum(table(control_final$semana_nueva) == table(control_anterior$semana))
# --- Debe dar TRUE 
(control_final %>% dim())[1] == (control_anterior %>% dim())[1]
# --- Zonal, debe dar 4
sum(table(control_final$zonal) == table(control_anterior$zonal))
# --- Control semanas
final %>%  
  group_by(periodo_nuevo) %>% 
  summarise(sem_min = min(semana_nueva),
            sem_max = max(semana_nueva))
# --- Control fondos rotativos: los periodos deben ser >= 4
final %>% filter(man_sec %in% fondo_rotativo$muestra) %>% 
  group_by(periodo_nuevo) %>% 
  summarise(n()) %>% 
  View()
# --- Control sectores peligrosos: los periodos deben ser >= 7
final %>% filter(id_upm %in% peligrosos$muestra) %>% 
  group_by(periodo_nuevo) %>% 
  summarise(n()) %>% 
  View()

# -----------------------------------------------------------------------------
# Exportando final
# -----------------------------------------------------------------------------


export(final %>% select(-estrato,-cambio_previo,-periodo,-semana,-control),
       "muestra_upm_man_sec_fondo_rot_004.xlsx")



