
rm(list = ls())

source("rutinas/99_librerias/librerias.R")
source("rutinas/99_librerias/read_zip.R")

# -----------------------------------------------------------------------------
# Lectura de la base de cobertura: Necesitamos indicar el periodo
# -----------------------------------------------------------------------------

periodo <- 9
periodo <- str_pad(periodo,2,"left","0")

#options(scipen = 999)

#-------------------------------------------------------------------------------
# Lectura marco UPM
#-------------------------------------------------------------------------------

marco_upm <- readRDS("insumos/02_muestra_upm/marco/marco_upm.rds")

# -----------------------------------------------------------------------------
# Lectura de la base de cobertura
#-------------------------------------------------------------------------------
# La base de cobertura solo tiene información de los hogares efectivos. 
# Por lo que las "viviendas" no efectivas serán NA en algunos merge con otras bases.
# p00: "Id de persona" (5 miembros = 1, 2 , 3 ,4 ,5)
# p03: Sexo
# p04: Años cumplidos
# id_persona: id_upm + vivienda + persona
#-------------------------------------------------------------------------------

cobertura_base_total_personas <- NULL

for(i in c(1:as.numeric(periodo))){
  
  print(i)

  periodo <- str_pad(i,2,"left","0")
  
  direc = paste0("insumos/04_cobertura/periodo_", periodo)
  archivo_zip = dir(direc)[grep(dir(direc), pattern = "Periodo")]
  documento = "F1_PERSONAS"
  
  cobertura_base_personas <- read_zip(direc, archivo_zip, documento) %>% 
    clean_names() 
  
  cobertura_base_total_personas <- rbind(cobertura_base_total_personas, cobertura_base_personas)
}

#table(cobertura_base_total_personas$conglo)

cobertura_base_total_personas <- cobertura_base_total_personas %>% 
  mutate(id_upm = str_pad(conglo, 12, "left", "0"), 
         hogar = str_pad(hogar, 2, "left", "0"), 
         persona = p00, 
         sexo = p03,
         edad = p04,
         id_upm_no_orden = paste0(id_upm, vivienda),
         id_persona = paste0(id_upm_no_orden, persona)) %>% 
  left_join(select(marco_upm, id_upm, estrato, pro, area), by = "id_upm") %>% 
  select(zonal, pro, id_upm, id_upm_no_orden, area, estrato, vivienda, hogar, 
         persona, id_persona, sexo, edad)

#-------------------------------------------------------------------------------
# Exportando base de personas
#-------------------------------------------------------------------------------

dir <-  paste0("intermedios/04_cobertura/")
rio::export(cobertura_base_total_personas,
     file = paste0(dir, "cobertura_base_total_personas.rds"))

