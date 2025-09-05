# -----------------------------------------------------------------------------
# geograf : La base geograf debe estar del perido completo en una misna base
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Lectura base de enlistamiento
# -----------------------------------------------------------------------------

periodo <- str_pad(periodo, 2, "left", "0")

ruta <- paste0("insumos/03_enlistamiento/periodo_", periodo)
n_base <- dir(ruta)[grepl(dir(ruta),pattern = ".csv")]
n_base <- ifelse(grepl(n_base,pattern = "ase_muestral"), n_base, "xxx")
base <- read.csv(paste0(ruta,"\\",n_base),
                 sep = ";",
                 encoding = "latin1") %>% 
  clean_names() %>% 
  mutate(c_ocup = tolower(c_ocup),
         primernjh = toupper(stri_trans_general(primernjh,"Latin-ASCII")),
         segundonjh	= toupper(stri_trans_general(segundonjh,"Latin-ASCII")),
         primerajh = toupper(stri_trans_general(primerajh,"Latin-ASCII")),
         segudonjh = toupper(stri_trans_general(segudonjh,"Latin-ASCII")),
         id_upm = upm,
         tot_hbt = as.numeric(tot_hbt))

# -----------------------------------------------------------------------------
# Controlando los caracteres en variables
# -----------------------------------------------------------------------------

base <- base %>% 
  mutate(pro = str_pad(pro, 2, "left", "0"),
         can = str_pad(can, 2, "left", "0"),
         par = str_pad(par, 2, "left", "0"),
         zon = str_pad(zon, 3, "left", "0"),
         sec = str_pad(sec, 3, "left", "0"),
         man = str_pad(man, 3, "left", "0"),
         n_loc = str_pad(n_loc, 3, "left", "0"),
         n_umce = str_pad(n_umce, 3, "left", "0"),
         n_viv = str_pad(n_viv, 4, "left", "0"),
         id_upm = str_pad(id_upm,12,"left","0"),
         convencional = ifelse(nchar(convencional)==9,str_pad(convencional, 10, "left", "0"),convencional),
         celular = ifelse(nchar(celular)==9,str_pad(celular, 10, "left", "0"),celular),
         man_nloc = ifelse(!is.na(man), man, n_loc),
         id_edif = paste0(pro, can, par, zon, sec, man_nloc, n_umce),
         id_viv = paste0(pro, can, par, zon, sec, man_nloc, n_umce, n_viv),
         primernjh = gsub(" ", "", primernjh),
         segundonjh = gsub(" ", "", segundonjh),
         primerajh = gsub(" ", "",primerajh),
         segudonjh =gsub(" ", "",segudonjh),
         tot_hbt = ifelse(is.na(tot_hbt) | tot_hbt == "", 0, tot_hbt)) %>% 
  replace(. == "",NA) %>% 
  mutate(man_sec_21 = ifelse(zon == "999", paste0(pro, can, par, zon, sec), 
                             paste0(pro, can, par, zon, sec, man)))

# -----------------------------------------------------------------------------
# Agregando semana y periodo
# -----------------------------------------------------------------------------

muestra <- import("productos/02_muestra_upm/muestra_upm_man_sec_fondo_rot_007.xlsx")
base <- base %>% left_join(muestra %>% filter(!duplicated(id_upm)) %>% 
                             select(id_upm,semana_nueva,periodo_nuevo),by="id_upm")

# -----------------------------------------------------------------------------
# Agregando estrato
# -----------------------------------------------------------------------------

marco_upm <- readRDS("insumos/02_muestra_upm/marco/marco_upm.rds")
base <- base %>% left_join(select(marco_upm,estrato,area,id_upm),by = "id_upm" )

# -----------------------------------------------------------------------------
# Agregando DPA
# -----------------------------------------------------------------------------

codif_dpa <- read_excel("insumos/99_nombres_dpa/CODIFICACIÓN_DPA_PAIS_2022_fin.xlsx") %>% 
  clean_names() %>% 
  rename(par = cod_parr_pais) %>% 
  select(c(1:4)) %>% 
  filter(!duplicated(par)) %>% 
  mutate(par = str_pad(par,6, "left", "0"),
         pro = substr(par,1,2),
         can = substr(par,3,4),
         par = substr(par,5,6))

base <- base %>% left_join(codif_dpa,by = c("pro","can","par"))

# -----------------------------------------------------------------------------
# Descargando base geografica
# -----------------------------------------------------------------------------

# base_geop_j2 <- read_sf("insumos/03_enlistamiento/2024_12_13/enighur_j2.gpkg",layer = "edif_p") %>% 
#   st_drop_geometry()
# 
# base_geop_j1 <- read_sf("insumos/03_enlistamiento/2024_11_25/enighur_j1.gpkg",layer = "edif_p") %>% 
#   st_drop_geometry()
# 
# base_geop_j1 <- base_geop_j1 %>% 
#   mutate (n_edif = str_pad(n_edif, 3, "left", "0"),
#           n_viv = str_pad(n_viv, 3, "left", "0"),
#           id_edif = ifelse(is.na(man), paste0(loc, n_edif), paste0(man, n_edif))) %>%  
#   select(id_edif, pluscodes = pluscodes, n_edif)
# 
# base_geop_j2 <- base_geop_j2 %>% 
#   mutate (n_edif = str_pad(n_edif, 3, "left", "0"),
#           n_viv = str_pad(n_viv, 3, "left", "0"),
#           id_edif = ifelse(is.na(man), paste0(loc, n_edif), paste0(man, n_edif))) %>%  
#   select(id_edif, pluscodes = pluscode, n_edif)
# 
# base_geop_j2 <- base_geop_j2 %>% filter(!id_edif %in% base_geop_j1$id_edif)
# 
# rbind(base_geop_j1,base_geop_j2) %>% dim()
# rbind(base_geop_j1,base_geop_j2) %>% filter(duplicated(id_edif)) %>% 
#   group_by(id_edif) %>% summarise(n_distinct(pluscodes)) %>% View()
#
#base_geop <- rbind(base_geop_j1,base_geop_j2)

n_gpkg <- dir(ruta)[grepl(dir(ruta),pattern = ".gpkg")]
#n_gpkg <- ifelse(grepl(n_gpkg,pattern = paste0("p",substr(periodo,2,2))),n_gpkg,"xxx")

base_geop <- read_sf(paste0("insumos/03_enlistamiento/","periodo_",periodo,"/",n_gpkg),layer = "edif_p") %>% 
  st_drop_geometry()

base_geop <- base_geop %>% 
  mutate (n_edif = str_pad(n_edif, 3, "left", "0"),
          n_viv = str_pad(n_viv, 3, "left", "0"),
          id_edif = ifelse(is.na(man), paste0(loc, n_edif), paste0(man, n_edif))) %>%  
  select(id_edif, pluscodes = pluscodes, n_edif)

# -----------------------------------------------------------------------------
# Agregando pluscode
# -----------------------------------------------------------------------------

base <- base %>% left_join(base_geop, by = "id_edif") 

base %>% filter(periodo_nuevo == as.numeric(periodo)) %>% 
  filter(is.na(pluscodes)) %>% group_by(id_upm) %>% summarise() %>% dim()
# viv sin pluscode
base %>% filter(periodo_nuevo == as.numeric(periodo)) %>% 
  filter(is.na(pluscodes)) %>% dim()
# upm sin pluscode
base %>% filter(periodo_nuevo == as.numeric(periodo)) %>% 
  filter(is.na(pluscodes)) %>% group_by(id_upm) %>% summarise() %>% dim()

# -----------------------------------------------------------------------------
# Agregando la variable: zonal
# -----------------------------------------------------------------------------

base <- base %>% 
  mutate(zonal = case_when(pro %in% c("04","08","10","17","21","25","30") ~ "ADM. C. CAMPO",
                           pro %in% c("02","05","06","15","16","18","22","29") ~ "CENTRO",
                           pro %in% c("09","12","13","20","23","24","26","32","33") ~ "LITORAL",
                           pro %in% c("01","03","11","07","14","19","27","28","31") ~ "SUR")) 

# -----------------------------------------------------------------------------
# Filtrando el periodo que necesitamos
# -----------------------------------------------------------------------------

base <- base %>% filter(periodo_nuevo == as.numeric(periodo))

if (unique(base$periodo_nuevo) != as.numeric(periodo)){
message("Error: Revisar el periodo en el que estamos")}

# -----------------------------------------------------------------------------
# Codigo para periodo 11
# -----------------------------------------------------------------------------

#base <- base %>% filter(id_upm != "010450900201")

# -----------------------------------------------------------------------------
# Exportando
# -----------------------------------------------------------------------------

ruta_export <- paste0("intermedios/03_enlistamiento/01_concistencia/","periodo_", periodo)
dir.create(ruta_export, showWarnings = F) 
dir.exists(ruta_export)
rio::export(base, paste0(ruta_export,"/base.rds"), overwrite = FALSE)

if( sum(grepl(dir(ruta_export), pattern = "base.rds")) == 1 ){
  print("--- Se exportó correctamente el documento base.rds ---")
}else{
  message("El código falló")
}

