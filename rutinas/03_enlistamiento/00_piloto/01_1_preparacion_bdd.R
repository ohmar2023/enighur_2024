# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------

rm(list = ls())

{
  library(rio)
  library(tidyverse)
  library(janitor)
  library(readxl)
}

base_piloto <- read_excel("insumos/03_enlistamiento/00_piloto/Base Prueba Piloto EnighurAC.XLSX") %>% 
  clean_names() %>% mutate (c_ocup = tolower(c_ocup),
                            id_upm = upm)

base <- base_piloto
# -----------------------------------------------------------------------------
# Agregando la variable: zonales
# -----------------------------------------------------------------------------

base <- base %>% 
  mutate(zonal = case_when(pro %in% c("04","08","10","17","21","25","30") ~ "norte",
                           pro %in% c("02","05","06","15","16","18","22","29") ~ "centro",
                           pro %in% c("09","12","13","20","23","24","26","32","33") ~ "litoral",
                           pro %in% c("01","03","11","07","14","19","27","28","31") ~ "sur")) 

# -----------------------------------------------------------------------------
# Agregando la variable: zonales
# -----------------------------------------------------------------------------

base <- base %>% 
  mutate(pro = str_pad(pro, 2, "left", "0"),
         can = str_pad(can, 2, "left", "0"),
         par = str_pad(par, 2, "left", "0"),
         zon = str_pad(zon, 3, "left", "0"),
         sec = str_pad(sec, 3, "left", "0"),
         man = str_pad(man, 3, "left", "0"),
         n_loc = str_pad(n_loc, 3, "left", "0"),
         n_umce = str_pad(n_umce, 4, "left", "0"),
         n_viv = str_pad(n_viv, 4, "left", "0"),
         tot_hbt = ifelse(is.na(tot_hbt) | tot_hbt == "", 0, tot_hbt)) %>% 
  replace(. == "",NA)

# -----------------------------------------------------------------------------
# Exportando
# -----------------------------------------------------------------------------

export(base, "intermedios/03_enlistamiento/01_concistencia/base.rds")

