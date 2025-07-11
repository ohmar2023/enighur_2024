rm(list = ls())

source("rutinas/99_librerias/librerias.R")
source("rutinas/99_librerias/read_zip.R")

# -----------------------------------------------------------------------------
# Lectura de la base de cobertura: Necesitamos indicar el periodo
# -----------------------------------------------------------------------------

periodo <- 7
periodo <- str_pad(periodo,2,"left","0")

# -----------------------------------------------------------------------------
# Lectura de la bases de cobertura de cada periodo
# -----------------------------------------------------------------------------

cobertura_base_total <- NULL

for(i in c(1:as.numeric(periodo))){
  periodo <- str_pad(i,2,"left","0")
  direc = paste0("intermedios/04_cobertura/periodo_", periodo,"/cobertura_base_periodo_", periodo, ".rds")
  #archivo_zip = dir(direc)[grep(dir(direc), pattern = "v1_")]
  #documento = "IDENTIFICACION"
  
  aux <- import(direc)
  
  cobertura_base_total <- rbind(cobertura_base_total,aux)
}

# -----------------------------------------------------------------------------
# Lectura de la bases de cobertura de cada periodo
# -----------------------------------------------------------------------------

ruta <- paste0("intermedios/04_cobertura/")
rio::export(cobertura_base_total, 
       paste0(ruta,"cobertura_base_total.rds"), overwrite = FALSE)



