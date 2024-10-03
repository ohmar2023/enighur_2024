# -----------------------------------------------------------------------------
# Se realiza el match con los codigos man_sec, este producto es el que se
# enviará como resultado de la muestra.
# -----------------------------------------------------------------------------

rm(list = ls())

{
library(rio)
library(tidyverse)
library(janitor)
}


man_sec_upm <- readRDS("insumos/02_muestra_upm/man_sec_upm.rds") %>% 
  rename(id_conglomerado=id_upm)

muestra <- readRDS("productos/02_muestra_upm/muestra_upm_periodo_semana.rds") %>% 
  mutate(id_conglomerado = substr(id_upm,1,10))

muestra_man_sec <- muestra %>% left_join(man_sec_upm,by="id_conglomerado") %>% 
  select(zonal,id_upm,man_sec,periodo,semana) %>% 
  group_by(man_sec) %>% 
  mutate(n = n()) %>% 
  ungroup()

n_distinct(muestra_man_sec$id_upm)
n_distinct(muestra_man_sec$man_sec)

envio <- muestra_man_sec %>% 
  mutate(pro = substr(man_sec,1,2),
         can = substr(man_sec,3,4),
         par = substr(man_sec,5,6),
         zon = substr(man_sec,7,9),
         sec = substr(man_sec,10,12),
         man = substr(man_sec,13,15)) %>% 
  select(-n) %>% 
  relocate(-"periodo",-"semana")


export(envio,"productos/02_muestra_upm/muestra_upm_man_sec.xlsx")
export(envio,"productos/02_muestra_upm/muestra_upm_man_sec.rds")

# -----------------------------------------------------------------------------
# viviendas por upm
# -----------------------------------------------------------------------------

viv_par_ocu_man_sec <- readRDS("D:/OMAR LLAMBO/enighur_2024/insumos/02_muestra_upm/viv_par_ocu_man_sec.rds")

envio_2 <- envio %>% left_join(viv_par_ocu_man_sec)
export(envio_2,"productos/02_muestra_upm/muestra_upm_man_sec_viv_par_ocu.xlsx")










