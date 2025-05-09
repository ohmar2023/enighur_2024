
# -----------------------------------------------------------------------------
# Lectura base enlistamiento, muestra
# -----------------------------------------------------------------------------

periodo <- str_pad(periodo, 2, "left", "0")
ruta <- paste0("intermedios/03_enlistamiento/01_concistencia/","periodo_",periodo)
base <- readRDS(paste0(ruta,"/base.rds"))

if(unique(base$periodo_nuevo) != as.numeric(periodo)){
  message("Error: Revisar el periodo en el que estamos")
}

muestra <- import("productos/02_muestra_upm/muestra_upm_man_sec_fondo_rot_006.xlsx")

# -----------------------------------------------------------------------------
# Lectura base de correspondencia y novedades - DICA
# -----------------------------------------------------------------------------

ruta_corr <- paste0("D:\\OMAR LLAMBO\\enighur_2024\\insumos\\03_enlistamiento\\periodo_", periodo)
n_base_corr <- dir(ruta_corr)[grepl(dir(ruta_corr),pattern = "esumen|orrespon")]

correspondencia <- read_excel(paste0(ruta_corr,"\\",n_base_corr)) %>% 
  clean_names() %>% 
  rename(id_upm = upm)

# -----------------------------------------------------------------------------
# mansec nuevos no levantados
# -----------------------------------------------------------------------------

man_sec_nuevos <- muestra %>% filter(periodo_nuevo == as.numeric(periodo)) %>%
  full_join(select(base,man_sec = man_sec_21,id_upm),
            by = c("man_sec","id_upm")) %>% 
  filter(is.na(zonal)) %>%  
  select(id_upm,man_sec) %>% 
  group_by(id_upm,man_sec) %>% 
  summarise() %>% 
  left_join(select(correspondencia,id_upm,
                   man_sec = clave_captura,estado_upm),
            by = c("id_upm","man_sec") ) %>% 
  filter(estado_upm != "COMPLETA" , estado_upm != "completa") 

# -----------------------------------------------------------------------------
# mansec en la muestra que no aparecen en el enlistamiento:
# -----------------------------------------------------------------------------

man_sec_no_enlistados <- muestra %>% 
  filter(!man_sec %in% base$man_sec_21, periodo_nuevo == as.numeric(periodo)) %>%
  select(id_upm,man_sec) %>% 
  group_by(id_upm,man_sec) %>% 
  summarise() %>% left_join(select(correspondencia,id_upm,
                                    man_sec = clave_muestra,estado_upm),
                             by = c("id_upm","man_sec") ) %>% 
  filter(estado_upm != "COMPLETA" , estado_upm != "completa") 

# -----------------------------------------------------------------------------
# UPM incompletas
# -----------------------------------------------------------------------------

ump_incompl <- muestra %>% filter(periodo_nuevo == as.numeric(periodo)) %>% 
  group_by(id_upm) %>% summarise(n_man_sec_m = n_distinct(man_sec)) %>% 
  left_join(
    base %>% group_by(id_upm) %>% 
      summarise(n_man_sec_b = n_distinct(man_sec_21)),
    by = "id_upm" ) %>% 
  mutate(dif = n_man_sec_m - n_man_sec_b) %>% 
  filter(dif > 0) %>% 
  arrange(dif)

# -----------------------------------------------------------------------------
# upm no enlistadas
# -----------------------------------------------------------------------------

upm_no_enlistadas <- muestra %>% 
  filter(!id_upm %in% base$id_upm, periodo_nuevo == as.numeric(periodo)) %>% 
  select(id_upm,man_sec) %>% 
  group_by(id_upm) %>% 
  summarise()

# -----------------------------------------------------------------------------
# upm que no aparecen en las novedades y no estan enlistadas
# -----------------------------------------------------------------------------

# muestra %>% filter(!id_upm %in% base$id_upm, periodo_nuevo==1) %>% 
#   select(id_upm,man_sec) %>% 
#   group_by(id_upm) %>% 
#   summarise() %>% 
#   filter(!id_upm %in% novedades$id_upm) %>% 
#   View()



# -----------------------------------------------------------------------------
# Exportando
# -----------------------------------------------------------------------------

dir <- paste0("intermedios\\03_enlistamiento\\01_concistencia\\","periodo_",periodo)
#dir.create(dir, showWarnings = F) 
dir.exists(dir)

wb <- createWorkbook("Inconsistencias base enlistamiento MANSEC-UPM")
addWorksheet(wb, "man_sec_nuevos")
addWorksheet(wb, "man_sec_no_enlistados")
addWorksheet(wb, "upm_incompletas")
addWorksheet(wb, "upm_no_enlistadas")

writeData(wb, sheet = "man_sec_nuevos", man_sec_nuevos)
writeData(wb, sheet = "man_sec_no_enlistados", man_sec_no_enlistados)
writeData(wb, sheet = "upm_incompletas", ump_incompl)
writeData(wb, sheet = "upm_no_enlistadas", upm_no_enlistadas)

saveWorkbook(wb, paste0(dir,"/", "incon_man_sec_upm_periodo_", periodo,".xlsx"), overwrite = T)

if(sum(dir(dir) == paste0("incon_man_sec_upm_periodo_", periodo,".xlsx"))==1){
  print(paste0("--- Se exportó correctamente el documento ",paste0("incon_man_sec_upm_periodo_", periodo,".xlsx"), " ---" ))
}else{
  message("El código falló")
}



