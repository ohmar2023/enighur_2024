
rm(list = ls())

{
  library(tidyverse)
  library(sf)
  library(inborutils)
  library(rio)
  library(tidyverse)
  library(janitor)
  library(readxl)
  library(openxlsx) 
}

# -----------------------------------------------------------------------------
# Lectura base enlistamiento, muestra y novedades
# -----------------------------------------------------------------------------
#st_layers("insumos/03_enlistamiento/2024_11_25/enighur_j1.gpkg")
base_geop <- read_sf("insumos/03_enlistamiento/2024_11_25/enighur_j1.gpkg",layer = "edif_p") %>% 
  st_drop_geometry()

# -----------------------------------------------------------------------------
# 
# -----------------------------------------------------------------------------

per = 1

base_geop <- base_geop %>% 
  mutate (n_edif = str_pad(n_edif, 3, "left", "0"),
          n_viv = str_pad(n_viv, 3, "left", "0"),
          id_edif = ifelse(is.na(man), paste0(loc, n_edif), paste0(man, n_edif)), 
          jornada = per) %>%  
  select(id_edif, pluscodes, jornada, n_edif)

base_geop %>% filter(is.na(pluscodes)) %>% dim()
#base_geop %>% filter(nchar(id_edif)==17) %>% View()

# -----------------------------------------------------------------------------
#  Agregando pluscode a OCUPADA
# -----------------------------------------------------------------------------

ocupada <- ocupada %>% 
  mutate(id_edif = paste0(pro, can, par, zon, sec, man_nloc, n_umce)) %>% 
  left_join(base_geop, by = "id_edif") 

# viv sin pluscode
  ocupada %>% filter(is.na(pluscodes)) %>% dim()

# upm sin pluscode
ocupada %>% filter(is.na(pluscodes)) %>% group_by(id_upm) %>% summarise() %>% dim()

