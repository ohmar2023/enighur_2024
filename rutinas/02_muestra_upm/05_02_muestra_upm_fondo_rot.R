# -----------------------------------------------------------------------------
# Se genera un base que permita identificar que UPM tienen fondo rotativo.
# -----------------------------------------------------------------------------


{
  library(rio)
  library(tidyverse)
  library(janitor)
  library(readxl)
  library(rio)
}

# -----------------------------------------------------------------------------
# Se genera un archivo que permita identificar que UPM tienen fondos rotativos
# -----------------------------------------------------------------------------

final <- read_excel("muestra_upm_man_sec_fondo_rot_002.xlsx") %>% rename(periodo = "periodo_nuevo")
final %>% group_by(id_upm) %>% summarise(n_distinct(periodo)) %>% View()


# --- UPM FONDO ROTATIVO
fondo_rotativo <- read_excel("insumos/02_muestra_upm/fondos_rotativos/FONDO ROTATIVO Formato ENIGHUR (002)_nacional.xlsx", 
                             sheet = "DETALLE SEC_MAN") %>% clean_names()

muestra_upm_periodo_fondo_rot <- final %>% 
  left_join(select(muestra,estrato,id_upm),by = "id_upm") %>% 
  mutate(cambio_sec = ifelse(man_sec %in% fondo_rotativo$muestra, TRUE, FALSE)) %>% 
  group_by(id_upm) %>% 
  mutate(fondo_rot_upm = ifelse(sum(cambio_sec) > 0,TRUE,FALSE)) %>% 
  ungroup() %>% 
  filter(!duplicated(id_upm)) %>% 
  select(zonal,id_upm,periodo,semana,estrato,fondo_rot_upm )


export(muestra_upm_periodo_fondo_rot,"muestra_upm_periodo_fondo_rot.xlsx")


  
  
  
  
  
  