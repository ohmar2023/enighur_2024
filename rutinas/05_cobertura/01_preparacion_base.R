
rm(list = ls())

source("rutinas/99_librerias/librerias.R")
source("rutinas/99_librerias/read_zip.R")

# -----------------------------------------------------------------------------
# Lectura de la base de cobertura: Necesitamos indicar el periodo
# -----------------------------------------------------------------------------

periodo <- 5
periodo <- str_pad(periodo,2,"left","0")

# -----------------------------------------------------------------------------
# Lectura de la base de cobertura
# -----------------------------------------------------------------------------

direc = paste0("insumos/04_cobertura/periodo_", periodo)
archivo_zip = dir(direc)[grep(dir(direc), pattern = as.numeric(periodo))]
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
# Exportando base
# -----------------------------------------------------------------------------

periodo <- str_pad(periodo, 2, "left", "0")
#ruta <- paste0("intermedios/04_cobertura/","periodo_",periodo)
ruta <- paste0("intermedios/04_cobertura/periodo_", periodo)
rio::export(cobertura_base, paste0(ruta,"/cobertura_base_periodo_", periodo,".rds"), overwrite = FALSE)

