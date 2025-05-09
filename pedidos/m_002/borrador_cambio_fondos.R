
rm(list = ls())

library(rio)
library(tidyverse)
library(janitor)
library(readxl)
library(rio)

# -----------------------------------------------------------------------------
# 
# -----------------------------------------------------------------------------

fondo_rotativo <- read_excel("insumos/02_muestra_upm/fondos_rotativos/FONDO ROTATIVO Formato ENIGHUR (002)_nacional.xlsx", 
                             sheet = "DETALLE SEC_MAN") %>% clean_names()

muestra_upm_man_sec <- import("productos/02_muestra_upm/muestra_upm_man_sec_fondo_rot_004.xlsx")

muestra <- readRDS("insumos/02_muestra_upm/muestra.rds")

# -----------------------------------------------------------------------------
# Lectura supermanzanas
# -----------------------------------------------------------------------------

upm_super_man <- read_excel("insumos/99_supermanzanas/upm_enighur_partir.xlsx")

# -----------------------------------------------------------------------------
# 
# -----------------------------------------------------------------------------

aux <- muestra_upm_man_sec %>% 
  left_join(select(muestra,estrato,id_upm),by = "id_upm") %>% 
  mutate(
    id_conglomerado = substr(id_upm,1,10),
    cambio_sec = ifelse(man_sec %in% fondo_rotativo$muestra &
                               periodo_nuevo %in% c(5,6) & 
                          substr(man_sec,1,2) != "20",TRUE,FALSE)) %>% 
  group_by(id_conglomerado) %>% 
  mutate(super_man = ifelse(id_conglomerado %in% upm_super_man$id_conglomerado,
                            "no","ok")) %>% 
  ungroup() %>% 
  group_by(id_upm) %>% 
  mutate(cambio_upm = ifelse(sum(cambio_sec) > 0,TRUE,FALSE),
        
         # cambio_upm = ifelse(id_upm %in% fondo_rotativo$muestra & substr(man_sec,1,2) == "20" 
         #                      & periodo_nuevo %in% c(5,6), TRUE, cambio_upm),
         #
         cambio_sec = ifelse(cambio_upm == TRUE,TRUE,cambio_sec),
         aux_cambio = n_distinct(man_sec),
         disp_upm = ifelse(cambio_upm == FALSE & 
                             sum(man_sec %in% fondo_rotativo$muestra) == 0 &
                             super_man == "ok" & 
                             periodo_nuevo >= 7 ,TRUE,FALSE)) %>% 
  ungroup() 
  #mutate(cambio_upm = ifelse(id_upm %in%  c("140953901301","140954900301"), FALSE, cambio_upm))

# -----------------------------------------------------------------------------
# 
# -----------------------------------------------------------------------------

id_upm_cambio <- aux %>% filter(cambio_upm == 1) %>%
  group_by(id_upm,estrato,periodo_nuevo,semana_nueva) %>% 
  summarise() %>% 
  ungroup() %>% 
  arrange(estrato)

id_upm_cambio %>% group_by(estrato) %>% summarise(n()) %>% 
  adorn_totals() %>% 
  View()

estrato_cambio <- id_upm_cambio %>% 
  group_by(estrato) %>% 
  summarise(n = n()) %>% 
  ungroup()

#set.seed(19092024)
set.seed(20250120)


id_upm_disponible <- aux %>% filter(disp_upm == TRUE) %>% 
  filter(!duplicated(id_upm)) %>% 
  filter(estrato != "2221") %>% 
  left_join(estrato_cambio, by = "estrato") %>% 
  filter(!is.na(n)) %>% 
  group_by(estrato) %>% 
  sample_n(unique(n)) %>% 
  arrange(estrato) %>% 
  ungroup()

# a <- cbind(id_upm_cambio,
#            periodo_nuevo = id_upm_disponible$periodo_nuevo,
#            semana_nueva = id_upm_disponible$semana_nueva)
# 
# b <- cbind(id_upm_disponible %>% select(-n),
#            periodo_nuevo = id_upm_cambio$periodo,
#            semana_nueva = id_upm_cambio$semana)
# 
# a_b <- rbind(a,b)

# -----------------------------------------------------------------------------
# Base resultante de cambios
# -----------------------------------------------------------------------------
id_upm_cambio %>% dim()
id_upm_disponible %>% dim()

cambios <- cbind(id_upm_cambio %>% filter(estrato %in% id_upm_disponible$estrato) %>% 
  arrange(estrato), id_upm_disponible %>% select(id_upm,
                                                 estrato,
                                                 periodo_nuevo)) %>% 
  as.data.frame() %>% clean_names() %>% 
  mutate(control = ifelse(estrato == estrato_2, "ok", "no"))

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

semana_antigua_tabla <- table(muestra_upm_man_sec$semana_nueva)
periodo_antigua_tabla <- table(muestra_upm_man_sec$periodo_nuevo,muestra_upm_man_sec$semana_nueva)



# -----------------------------------------------------------------------------
# USANDO FUNCION
# -----------------------------------------------------------------------------

p <- muestra_upm_man_sec %>% 
  rename(periodo = periodo_nuevo,semana = semana_nueva) %>% 
  mutate( periodo_nuevo = periodo,
          semana_nueva = semana)

for (i in c(1:dim(cambios)[1])) {
  p <- cambiar_upm(cambios$id_upm[i],cambios$id_upm_2[i],p)
  message(i)
}

p <- p %>% mutate(control_per = ifelse(periodo == periodo_nuevo,1,0),
                  control_sem = ifelse(semana == semana_nueva,1,0)) 

# debe dar 16 x2 = 32
p %>% filter(!duplicated(id_upm), control_per == 0) %>% dim()
p %>% filter(!duplicated(id_upm), control_sem == 0) %>% dim()

# -----------------------------------------------------------------------------
# UPM QUE FALTAN CAMBIAR
# -----------------------------------------------------------------------------

#export(aux,"aux_.xlsx")
#dir()

id_upm_cambio %>% filter(!estrato %in% id_upm_disponible$estrato)

# 140852900101 1421 - cambio por: 141052000101 1422
# 
# 140953901301 1421 - cambio por: 140850000501 1423
# 
# 140954900601 1421 - cambio por: 140258000101 1423
# 
# 220155900101 2221 - cambio por: 220357900401 2222
# 
# 220253900101 2221 - cambio por: 220350900401 2222

p <- cambiar_upm("140852900101","140852900101",p)
p <- cambiar_upm("140953901301","140850000501",p)
p <- cambiar_upm("140954900601","140258000101",p)
p <- cambiar_upm("220155900101","220357900401",p)
p <- cambiar_upm("220253900101","220350900401",p)


muestra_upm_man_sec %>% group_by(zonal) %>% summarise(n())
p %>% group_by(zonal) %>% summarise(n())

muestra_upm_man_sec %>% 
  filter(!duplicated(id_upm)) %>% 
  group_by(periodo_nuevo) %>% summarise(n_ant = n()) %>% 
  left_join(p %>% filter(!duplicated(id_upm)) %>% 
              group_by(periodo_nuevo) %>% summarise(n_nuev = n()), by = "periodo_nuevo") %>% 
  mutate(control = ifelse(n_ant == n_nuev,"ok","mal")) %>% View()

muestra_upm_man_sec %>% 
  filter(!duplicated(id_upm)) %>% 
  group_by(semana_nueva) %>% summarise(n_ant = n()) %>% 
  left_join(p %>% filter(!duplicated(id_upm)) %>% 
              group_by(semana_nueva) %>% summarise(n_nuev = n()), by = "semana_nueva") %>% 
  mutate(control = ifelse(n_ant == n_nuev,"ok","mal")) %>% View()


export(p, "muestra_upm_man_sec_fondo_rot_006.xlsx")


r <- read_excel("muestra_upm_man_sec_fondo_rot_005.xlsx")
#r <- muestra_upm_man_sec

p %>% group_by(id_upm,periodo_nuevo) %>% summarise(per_nuevo = unique(periodo_nuevo)) %>% 
  left_join(r %>% group_by(id_upm,periodo_nuevo) %>% summarise(per_05 = unique(periodo_nuevo)), by = "id_upm") %>% 
  mutate(control = ifelse(per_nuevo==per_05,"igual", "diff")) %>% 
  View()

p_1 <- p %>% select("zonal", "id_upm","man_sec", "pro", "can", "par", "zon", "sec", 
             "man", "periodo_nuevo", "semana_nueva" ) %>% 
  group_by(zonal, id_upm, semana_nueva,periodo_nuevo) %>% 
  summarise() 

p_1 %>% group_by(periodo_nuevo,semana_nueva) %>% summarise(n()) %>% View()

export(p_1, "muestra_upm_fondo_rot_006.xlsx")


