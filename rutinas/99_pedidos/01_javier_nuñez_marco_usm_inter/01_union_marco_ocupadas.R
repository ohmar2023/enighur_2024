
rm(list = ls())

source("rutinas/99_librerias/librerias.R")

# ------------------------------------------------------------------------------
#
# ------------------------------------------------------------------------------

muestra_usm_intermedia <- NULL

for (i in c(3:7)){
  periodo <- 4
  periodo <- str_pad(periodo, 2, "left", "0")
  ruta <- paste0("productos/02_muestra_usm/","periodo_", periodo,"/muestra_usm_inter.rds")
  aux <- import(ruta)
  muestra_usm_intermedia <- rbind(muestra_usm_intermedia, aux)
}

export(muestra_usm_intermedia, "muestra_usm_intermedia.rds")

