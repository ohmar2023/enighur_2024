
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

muestra_upm_man_sec <- readRDS("productos/02_muestra_upm/muestra_upm_man_sec.rds")
muestra_upm_periodo_semana <- readRDS("productos/02_muestra_upm/muestra_upm_periodo_semana.rds")

# -----------------------------------------------------------------------------
# Lectura base de fondos rotativos
# -----------------------------------------------------------------------------

fondo_rotativo <- read_excel("insumos/02_muestra_upm/fondos_rotativos/FONDO ROTATIVO Formato ENIGHUR (002)_nacional.xlsx", 
                             sheet = "DETALLE SEC_MAN") %>% clean_names()
  
aux <- muestra_upm_man_sec %>% 
  left_join(select(muestra,estrato,id_upm),by = "id_upm") %>% 
  mutate(cambio_sec = ifelse(man_sec %in% fondo_rotativo$muestra &
           periodo %in% c(1,2,3) &
           substr(id_upm,1,2)!="20",TRUE,FALSE)) %>% 
  group_by(id_upm) %>% 
  mutate(cambio_upm = ifelse(sum(cambio_sec) > 0,TRUE,FALSE),
         cambio_sec = ifelse(cambio_upm == TRUE,TRUE,cambio_sec),
         aux_cambio = n_distinct(man_sec),
         super_man = substr(id_upm,11,12),
         disp_upm = if_else(cambio_upm == 0 & 
                              super_man == "01" & 
                              periodo > 3,1,0)) %>% 
  ungroup() 

# -----------------------------------------------------------------------------
# Bases IDS x UPM
# -----------------------------------------------------------------------------

id_upm_cambio <- aux %>% filter(cambio_upm == 1) %>%
  group_by(id_upm,estrato,periodo) %>% 
  summarise() %>% 
  ungroup() %>% 
  arrange(estrato)

estrato_cambio <- id_upm_cambio %>% 
  group_by(estrato) %>% 
  summarise(n = n()) %>% 
  ungroup()

set.seed(19092024)

id_upm_disponible <- aux %>% filter(cambio_upm == 0 ) %>% 
  group_by(id_upm,estrato,periodo) %>% 
  summarise() %>% 
  ungroup() %>% 
  left_join(estrato_cambio, by = "estrato") %>% 
  filter(!is.na(n)) %>% 
  group_by(estrato) %>% 
  sample_n(unique(n)) %>% 
  arrange(estrato)
  
# -----------------------------------------------------------------------------
# Periodos nuevos
# -----------------------------------------------------------------------------

a <- cbind(id_upm_cambio,
      periodo_nuevo = id_upm_disponible$periodo)

b <- cbind(id_upm_disponible %>% select(-n),
      periodo_nuevo = id_upm_cambio$periodo)

a_b <- rbind(a,b)

# -----------------------------------------------------------------------------
# Periodos nuevos
# -----------------------------------------------------------------------------

final <- muestra_upm_man_sec %>% 
  left_join(select(a_b, periodo_nuevo,id_upm),by = c("id_upm")) %>% 
  mutate(periodo_nuevo = ifelse(is.na(periodo_nuevo),periodo,periodo_nuevo),
         control = periodo == periodo_nuevo) 

# --- upm faltante
aux_2 <- final %>% filter(control == FALSE)
aux_2 %>% filter(id_upm %in% a$id_upm) %>% View()

n_distinct(aux_2$id_upm)
dim(a_b)
(aux_2)

export(final,"muestra_upm_man_sec_fondo_rot.xlsx")

final %>% group_by(semana) %>% summarise(n()) %>% view()
muestra_upm_man_sec %>% group_by(semana) %>% summarise(n()) %>% view()




