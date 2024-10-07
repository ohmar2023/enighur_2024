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
  clean_names() %>% mutate (c_ocup = tolower(c_ocup))

base <- base_piloto
# -----------------------------------------------------------------------------
# Agregando la variable: zonale
# -----------------------------------------------------------------------------

baseo <- base %>% 
  mutate(zonal = case_when(pro %in% c("04","08","10","17","21","25","30") ~ "norte",
                           pro %in% c("02","05","06","15","16","18","22","29") ~ "centro",
                           pro %in% c("09","12","13","20","23","24","26","32","33") ~ "litoral",
                           pro %in% c("01","03","11","07","14","19","27","28","31") ~ "sur")) 

# -----------------------------------------------------------------------------
# Condición de ocupación
# -----------------------------------------------------------------------------

base %>% 
  group_by(condicón = c_ocup,zonal) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = zonal, values_from = n) %>% 
  adorn_totals(c("col")) %>%
  arrange(condicón) %>% 
  View()

validUTF8(base_piloto$c_ocup) %>% sum()

# -----------------------------------------------------------------------------
# Control de las variables de ubicación geográfica.
# -----------------------------------------------------------------------------

inconsistencia <-
  mutate(c_na_pro = ifelse(is.na(pro), 1, 0),
         c_in_pro = ifelse((as.numeric(pro) %in% 1:24), 1, 0),
         c_na_can = ifelse(is.na(can), 1, 0),
         c_in_can = ifelse((as.numeric(can) %in% 1:99), 1, 0),
         c_na_par = ifelse(is.na(par), 1, 0),
         c_in_par = ifelse((as.numeric(par) %in% 50:99), 1, 0),
         c_na_zon = ifelse(is.na(zon), 1, 0),
         c_in_zon = ifelse((as.numeric(zon) %in% 1:999), 1, 0),
         c_na_sec = ifelse(is.na(sec), 1, 0),
         c_in_sec = ifelse((as.numeric(sec) %in% 1:999), 1, 0),
         c_na_manloc = ifelse(is.na(man), 1, 0),
         c_in_manloc = ifelse((as.numeric(n_loc) %in% 1:999), 1, 0),
         
         c_na_n_edif = ifelse(is.na(d_n_edif), 1, 0),
         c_in_n_edif = ifelse(!(as.numeric(d_n_edif) %in% 1:999), 1, 0),
         c_na_n_viv = ifelse(is.na(d_n_viv), 1, 0),
         c_in_n_viv = ifelse(!(as.numeric(d_n_viv) %in% 1:999), 1, 0),
         c_na_piso_n = ifelse(is.na(piso_n), 1, 0),
         c_in_piso_n = 0,
         no_id_unico = ifelse(no_id_unico > 1, 1, 0),
         n0a5 = cero_dos + dos_cinco,
         control_nombre = ifelse(substr(primernjh, 1, 5) == "nadie", "Nadie en casa",
                                 ifelse(primernjh == "rechazo", "Rechazo",
                                        ifelse(primernjh == "nn", "Sin Nombre", "Con nombre"))),
         tipo = ifelse(n0a5 > 0, "Tiene niño",
                       ifelse(control_nombre != "Con nombre", control_nombre, "No tiene niño")),
         c_hbt = ifelse(n0a5 >= n_hbt, 1, 0),
         c_n0a5 = ifelse(n0a5 > 3, 1, 0),
         c_12_hbt = ifelse(n_hbt > 12, 1, 0),
         c_no_upm = ifelse(is.na(id_upm), 1, 0)) %>% 
  select(-d_n_edif, -d_n_viv, -man_nloc, -id_viv, -n0a5, -control_nombre, -tipo, -id_upm, -sec_2021) %>% 
  mutate(control_total = no_id_unico + c_na_pro + c_in_pro + c_na_can + c_in_can + c_na_par + 
           c_in_par + c_na_zon + c_in_zon + c_na_sec + c_in_sec + c_na_manloc + c_in_manloc + 
           c_na_n_edif + c_in_n_edif + c_na_n_viv + c_in_n_viv + c_na_piso_n + c_in_piso_n + 
           c_hbt + c_n0a5 + c_12_hbt + c_no_upm) %>% 
  filter(control_total > 0) %>% 
  select(-control_total)



