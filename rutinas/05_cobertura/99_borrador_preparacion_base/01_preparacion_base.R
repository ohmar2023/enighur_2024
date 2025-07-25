
rm(list = ls())

source("rutinas/99_librerias/librerias.R")
source("rutinas/99_librerias/read_zip.R")

# -----------------------------------------------------------------------------
# Lectura de la base de cobertura: Necesitamos indicar el periodo
# -----------------------------------------------------------------------------

periodo <- 6
periodo <- str_pad(periodo,2,"left","0")

# -----------------------------------------------------------------------------
# Lectura de la base de cobertura
# -----------------------------------------------------------------------------

direc = paste0("insumos/04_cobertura/periodo_", periodo)
archivo_zip = dir(direc)[grep(dir(direc), pattern = "Periodo")]
archivo_zip
documento = "IDENTIFICACION"

cobertura_base <- read_zip(direc, archivo_zip, documento) %>% 
  clean_names() %>% 
  mutate(ciudad = remove_labels(ciudad))

# -----------------------------------------------------------------------------
# Limpieza de la base
# -----------------------------------------------------------------------------

cobertura_base <- cobertura_base %>% 
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
# Corrigiendo hogar 05: Estas son encuestas recuperadas por los equipos "bonberos"
# Agregando la variable ELEGIBILIDAD
# -----------------------------------------------------------------------------

cobertura_base_1 <- cobertura_base %>% 
  filter(ifelse(hogar %in%  c(5,6) & rvo != 1, FALSE, TRUE)) %>%
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
                                  TRUE ~ "error")) 


# -----------------------------------------------------------------------------
# Este bloque de código evula los hogares 05 que son levanatdos por los boomberos
# -----------------------------------------------------------------------------

#   group_by(id_upm_no_orden) %>% 
#   mutate(revision_1 = 1, 
#          aux_1 = row_number(),
#          aux_2 = n(),
#          revision_2 = ifelse (aux_2 > 1 & n_rvo != "Completa" & hogar == 5, 0, revision_1)) %>% 
#   ungroup() %>% 
#   filter(revision_2 == 1) %>% 
#   select(-aux_1, -aux_2, -revision_1, -revision_2)
# 
# # Controlando estas viviendas catalogadas como hogar 5
# 
# control_hog_05 <- table(cobertura_base$hogar) %>% data.frame() %>% 
#   rename("n_original" = Freq) %>% 
#   left_join(table(cobertura_base_1$hogar) %>% 
#               data.frame() %>% 
#               rename("n_nuevo" = Freq) , by = "Var1") %>% 
#   mutate(control = n_original - n_nuevo)
# 
# dim(cobertura_base)[1] - dim(cobertura_base_1)[1]

# -----------------------------------------------------------------------------
# Exportando base
# -----------------------------------------------------------------------------

periodo <- str_pad(periodo, 2, "left", "0")
ruta <- paste0("intermedios/04_cobertura/periodo_", periodo)
rio::export(cobertura_base_1, paste0(ruta,"/cobertura_base_periodo_", periodo,".rds"), overwrite = FALSE)

