
# -----------------------------------------------------------------------------
# Este script compila los demás. Se requieren parametros de periodo y fecha
# -----------------------------------------------------------------------------

rm(list = ls())

source("rutinas/99_librerias/librerias.R")

# -----------------------------------------------------------------------------
# Parámetros
# -----------------------------------------------------------------------------

periodo <- 13
# año/mes/dia
fecha <- "2025_10_13"

# -----------------------------------------------------------------------------
# script: 01_1_preparacion_bdd.R
# Este script llama las bases de datos del enlistamiento: 
# - Base_muestral__ XXXXXXXX_xxxxxxx.csv: Base alfanumerica del enlistamiento
# - nac_enighur_xx.gpkg: Base geografica para sacar los pluscode. Nos envian y hay que descomprimir. 
# Retorna: base.rds, esta base es la base alfanumerica lista para usarse
# -----------------------------------------------------------------------------

source("rutinas/03_enlistamiento/01_preparacion_bdd.R")

# -----------------------------------------------------------------------------
# script: 02_1_novedades.R
# Este script solo necesita base.rds
# Identifica novedades con respecto a las variables gegráficas
# Retrona fecha.xlsx: Un excel con las novedades encontradas. Por lo general solo hay novedad en c_12_hbt.
# -----------------------------------------------------------------------------

source("rutinas/03_enlistamiento/02_1_novedades.R")

# -----------------------------------------------------------------------------
# script: 02_2_novedades.R
# Este script solo necesita base.rds y ResumenUPMCorrespondenciaxxxxx.xlsx
# Identifica novedades con respecto a las manzanas enviadas y recibidas
# Retrona incon_man_sec_upm_periodo_07.xlsx: Un excel con las novedades encontradas.
#  Se revisa todas las pestañas.
# -----------------------------------------------------------------------------

source("rutinas/03_enlistamiento/02_2_novedades.R")




