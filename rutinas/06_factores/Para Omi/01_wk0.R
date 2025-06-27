#
rm(list = ls())

library(rio)
library(tidyverse)

# cargamos base de cobertura
mes <- list.files("productos/05_cobertura")
mes

# para que mes es
m <- 6

load(paste0("intermedios/05_cobertura/", mes[m], "/campo.RData"))

##### Proceso previo #####
# mover el marco upm del mes correspondiente desde marco_administracion a enciet
# proceso hasta 202501
# seleccion_upm <- list.files("../marco_administracion/productos/02_seleccion/03_enciet", full.names = T)
# 
# if(file.exists(paste0("intermedios/06_factores/", mes[m])) == F){
#   dir.create(paste0("intermedios/06_factores/", mes[m]), showWarnings = F)
#   file.copy(list.files(seleccion_upm[m], full.names = T),
#             paste0("intermedios/06_factores/", mes[m], "/pik_upm.rds"))
#   
#   print(mes[m]);
#   print(paste0("Muestra copiada: ", list.files(seleccion_upm[m+1], full.names = T)))
#   
# } else{
#   file.copy(list.files(seleccion_upm[m], full.names = T),
#             paste0("intermedios/06_factores/", mes[m], "/pik_upm.rds"))
#   print(mes[m]);
#   print(paste0("Muestra copiada: ", list.files(seleccion_upm[m+1], full.names = T)))
# }
# 
# rm(seleccion_upm)
##### FIN #####

##### Proceso actual
#### nacional sin galapagos #### 
auxiliar <- cob_viv %>% 
  group_by(id_upm) %>% 
  summarise(totper = sum(totper),
            tothog = sum(nhog))

aggr_cobertura_upm <- cob_viv %>% 
  group_by(id_upm, re) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = re, values_from = freq) %>% 
  replace(is.na(.), 0) %>% 
  rename_all(tolower) %>% 
  left_join(auxiliar, by = "id_upm")
# filter(substr(id_upm, 1, 2) != "20")

aggr_marco_upm_20 <- readRDS("productos/02_marcos/aggr_marco_upm_20.rds")

upm_pik_20 <- muestra %>% 
  mutate(pro = substr(id_upm, 1, 2)) %>% 
  filter(pro == "20") %>% 
  left_join(aggr_marco_upm_20, by = "estrato") %>% 
  group_by(estrato) %>% 
  mutate(mh = n()) %>% 
  ungroup() %>% 
  mutate(pik = mh/Nh) %>% 
  select(id_upm, pro, area, estrato, pik, mh)

upm_pik <- readRDS("intermedios/06_factores/pik_upm.rds") %>% 
  filter(id_upm %in% muestra$id_upm) %>% 
  rename(pik = ends_with("_pii")) %>% 
  group_by(estrato) %>% 
  mutate(mh = n_distinct(id_upm)) %>% 
  ungroup() %>% 
  select(-Mi) %>% 
  rbind(upm_pik_20)

conteo_20 <- readRDS("productos/02_marcos/aggr_marco_upm_20.rds")

aggr_marco_upm_20 <- muestra %>% 
  filter(substr(id_upm, 1, 2) == "20") %>% 
  left_join(conteo_20, by = "estrato") %>% 
  select(id_upm, estrato, Nh, Ni)

aggr_marco_upm <- readRDS("productos/02_marcos/aggr_marco_upm.rds") %>% 
  filter(id_upm %in% upm_pik$id_upm,
         substr(id_upm, 1, 2) != "20") %>% 
  rbind(aggr_marco_upm_20)

aggr_muestra_upm <- muestra %>% 
  group_by(id_upm) %>% 
  summarise(ki = n()) %>% 
  ungroup()

rm(aggr_marco_upm_20, conteo_20, upm_pik_20)

wk0 <- upm_pik %>% 
  full_join(aggr_marco_upm, by = c("estrato", "id_upm")) %>% 
  full_join(aggr_muestra_upm, by = "id_upm") %>% 
  full_join(aggr_cobertura_upm, by = "id_upm") %>% 
  mutate(ki_p = tre + tne + ted + tnr)

colSums(is.na(wk0))

# Se controla que el número de viviendas por UPM coincida entre la muestra y la base de cobertura
# Se verifica que la base wk0 este a nivel de UPM siendo sus registros únicos
# SE PASAN LOS CONTROLES CUANDO SON 0
table(wk0$ki_p, useNA = "ifany")

# Los perdidos corresponden a
# 2024_01: 8 upm no presentes en la base, 1 upm incompleta (4 viv faltantes)
dim(wk0)[1] - dim(aggr_cobertura_upm)[1]
dim(wk0)[1] - n_distinct(wk0$id_upm)

dir.create(paste0("productos/06_factores/01_mensual/", mes[m]), showWarnings = F)

#### creacion de la wk0 total ####

save(wk0, file = paste0("productos/06_factores/01_mensual/", mes[m], "/wk.RData"))
