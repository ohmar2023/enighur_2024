
rm(list = ls())

source("rutinas/99_librerias/librerias.R")

# -----------------------------------------------------------------------------
# El presente script solo genera dos tablas y se usan en el rmarkdown
# El markdown solo muestra la base acumulada
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Lectura base de cobertura
# -----------------------------------------------------------------------------

periodo <- 11
periodo <- str_pad(periodo, 2, "left", "0")

# -----------------------------------------------------------------------------
# Condición de ocupación - acumulada
# Se filtra un hogar por vivienda, cuyo resultado de encuesta será el min(rvo).
# -----------------------------------------------------------------------------

ruta <- paste0("intermedios/04_cobertura/")

cobertura_base_total <- import(paste0(ruta,"cobertura_base_total.rds")) %>% 
  filter(!is.na(rvo)) %>% 
  group_by(id_upm, id_upm_no_orden) %>% 
  mutate(n_fila = row_number(), 
         i_menor = which.min(rvo)) %>%
  filter(n_fila == unique(i_menor)) %>% 
  ungroup() 

# -----------------------------------------------------------------------------
# Validando base de datos cobertura base total
# -----------------------------------------------------------------------------

control_cobertura <- cobertura_base_total %>% 
  group_by(zonal, id_upm, Elegibilidad) %>% 
  summarise( n = n() ) %>% 
  pivot_wider(names_from = Elegibilidad, 
              values_from = n) 
control_cobertura[is.na(control_cobertura)] <- 0

control_cobertura <- control_cobertura %>% 
  mutate(control_12 = ed + ne + nr + re)  

#control_cobertura %>% View("control_cobertura")

# -----------------------------------------------------------------------------
# Tabla condición de ocupación
# -----------------------------------------------------------------------------

c_ocup_acum <- cobertura_base_total %>% 
  group_by(zonal, n_rvo) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = zonal, values_from = n) %>% 
  arrange(n_rvo) %>% 
  rename("Condición Ocupación" = n_rvo)

c_acum_aux <- c_ocup_acum[,c(2:5)]
c_acum_aux[is.na(c_acum_aux)] <- 0
c_ocup_acum <- cbind(c_ocup_acum[,1], c_acum_aux)

#cobertura_base_total %>% group_by(zonal) %>% summarise(n())



