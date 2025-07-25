
rm(list = ls())

source("rutinas/99_librerias/librerias.R")
source("rutinas/99_librerias/read_zip.R")

# -----------------------------------------------------------------------------
# Lectura de la base de cobertura: Necesitamos indicar el periodo
# -----------------------------------------------------------------------------

periodo <- 7
periodo <- str_pad(periodo,2,"left","0")

# -----------------------------------------------------------------------------
# Lectura de la base de cobertura
# -----------------------------------------------------------------------------


cobertura_base_total <- NULL

for(i in c(1:as.numeric(periodo))){
  
  periodo <- str_pad(i,2,"left","0")
  
  direc = paste0("insumos/04_cobertura/periodo_", periodo)
  archivo_zip = dir(direc)[grep(dir(direc), pattern = "Periodo")]
  archivo_zip
  documento = "IDENTIFICACION"
  
  cobertura_base <- read_zip(direc, archivo_zip, documento) %>% 
    clean_names() %>% 
    mutate(ciudad = remove_labels(ciudad))
  
  cobertura_base_total <- rbind(cobertura_base_total, cobertura_base)
}

# -----------------------------------------------------------------------------
# Limpieza de la base
# -----------------------------------------------------------------------------

cobertura_base_total <- cobertura_base_total %>% 
  mutate( conglo =  str_pad(conglo, 12, "left", "0"),
          provincia = str_pad(provincia, 2, "left", "0"),
          ciudad = str_pad(ciudad, 6, "left", "0"),
          zona = str_pad(zona, 3, "left", "0"),
          sector = str_pad(sector, 3, "left", "0"),
          vo = str_pad(vo, 4, "left", "0"), #num de vivienda
          manzana = str_pad(manzana, 3, "left", "0"),
          edificio = gsub(pattern = "-", replacement = "",edificio),
          edificio = str_pad(edificio, 3, "left", "0")) %>% 
  rename( id_upm = conglo, 
          pro = provincia,
          zon = zona,
          sec = sector,
          man_nloc = manzana,
          n_umce = edificio,
          n_viv = vo) %>% 
  mutate(can = substr(ciudad,3,4), 
         par = substr(ciudad,5,6)) %>% 
  mutate( id_edif = paste0(pro, can, par, zon, sec, man_nloc, n_umce),
          id_viv = paste0(pro, can, par, zon, sec, man_nloc, n_umce, n_viv),
          zonal = case_when(pro %in% c("04","08","10","17","21","25","30") ~ "ADM. C. CAMPO",
                            pro %in% c("02","05","06","15","16","18","22","29") ~ "CENTRO",
                            pro %in% c("09","12","13","20","23","24","26","32","33") ~ "LITORAL",
                            pro %in% c("01","03","11","07","14","19","27","28","31") ~ "SUR")) 

# -----------------------------------------------------------------------------
# Agregando la variable ELEGIBILIDAD
# -----------------------------------------------------------------------------

cobertura_base_total <- cobertura_base_total %>%
  mutate(id_upm_no_orden = paste0(id_upm, vivienda), #se usa como auxiliar para match de bases con DIES
         rvo = ifelse(rvo == 10, 2, rvo)) %>%  #la categoría 10 solo fue hecha para el periodo 01
  mutate(n_rvo = case_when(rvo == 1 ~ "Completa",
                           rvo == 2 ~ "Rechazo",
                           rvo == 3 ~ "Nadie en casa",
                           rvo == 4 ~ "Vivienda temporal",
                           rvo == 5 ~ "Vivienda desocupada",
                           rvo == 6 ~ "Vivienda en construcción",
                           rvo == 7 ~ "Vivienda inhabitada o destruida",
                           rvo == 8 ~ "Vivienda convertida en negocio",
                           rvo == 9 ~ "Otra razón",
                           TRUE ~ "error")) %>% 
  mutate(Elegibilidad = case_when(rvo == 1 ~ "re",
                                  rvo == 2 ~ "nr",
                                  rvo %in% c(4,5,6,7,8,9) ~ "ne",
                                  rvo == 3 ~ "ed",
                                  TRUE ~ "error")) %>% 
  mutate(id_conglomerado = substr(id_upm,1,10))

# -----------------------------------------------------------------------------
# Controles generales
# -----------------------------------------------------------------------------

table(cobertura_base_total$periodo)
n_distinct(cobertura_base_total$id_upm_no_orden)

# -----------------------------------------------------------------------------
# Exportando base
# -----------------------------------------------------------------------------

ruta <- paste0("intermedios/04_cobertura/")
rio::export(cobertura_base_total, 
            paste0(ruta,"cobertura_base_total.rds"), overwrite = FALSE)



# cobertura_base_total %>%
#   group_by(id_upm_no_orden) %>%
#   mutate( n = n()) %>%
#   ungroup() 
#   #filter(n > 1, zonal == "CENTRO") %>%
#   
#   cobertura_base_total %>% 
#   group_by(id_upm, n_rvo) %>%
#   summarise(n = n()) %>%
#   pivot_wider(names_from = n_rvo,
#               values_from = n) %>%
#   filter(Completa < 6 ) %>% 
#   View()

