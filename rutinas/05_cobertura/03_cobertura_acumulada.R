rm(list = ls())

source("rutinas/99_librerias/librerias.R")
source("rutinas/99_librerias/read_zip.R")

# -----------------------------------------------------------------------------
# Lectura de la base de cobertura: Necesitamos indicar el periodo
# -----------------------------------------------------------------------------

periodo <- 3
periodo <- str_pad(periodo,2,"left","0")

# -----------------------------------------------------------------------------
# Lectura de la bases de cobertura de cada periodo
# -----------------------------------------------------------------------------

cobertura_base_total <- NULL

for(i in c(1:as.numeric(periodo))){
  periodo <- str_pad(i,2,"left","0")
  direc = paste0("insumos/04_cobertura/periodo_", periodo)
  archivo_zip = dir(direc)[grep(dir(direc), pattern = "v1_")]
  documento = "IDENTIFICACION"
  
  aux <- read_zip(direc, archivo_zip, documento) %>% 
    clean_names() %>% 
    mutate(ciudad = remove_labels(ciudad))
  
  cobertura_base_total <- rbind(cobertura_base_total,aux)
}

# -----------------------------------------------------------------------------
# Lectura de la bases de cobertura de cada periodo
# -----------------------------------------------------------------------------

cobertura_base_total <- cobertura_base_total %>% 
  mutate(n_rvo = case_when(rvo == 1 ~ "Completa",
                           rvo == 2 ~ "Rechazo",
                           rvo == 3 ~ "Nadie en casa",
                           rvo == 4 ~ "Vivienda temporal",
                           rvo == 5 ~ "Vivienda desocupada",
                           rvo == 6 ~ "Vivienda en construcción",
                           rvo == 7 ~ "Vivienda inhabitada o destruida",
                           rvo == 8 ~ "Vivienda convertida en negocio",
                           rvo == 9 ~ "Otra razón",
                           rvo == 10 ~ "Rechazo a mitad de la encuesta")) %>% 
  mutate(Elegibilidad = case_when(rvo == 1 ~ "re",
                                  rvo == 2 ~ "nr",
                                  rvo %in% c(4,5,6,7,8,9) ~ "ne",
                                  rvo == 3 ~ "ed",
                                  TRUE ~ "error"))
  
# -----------------------------------------------------------------------------
# Lectura de la bases de cobertura de cada periodo
# -----------------------------------------------------------------------------

ruta <- paste0("intermedios/04_cobertura/")
export(cobertura_base_total, 
       paste0(ruta,"cobertura_base_total.rds"), overwrite = FALSE)



