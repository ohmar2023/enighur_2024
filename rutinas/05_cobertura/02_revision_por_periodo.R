
rm(list = ls())

source("rutinas/99_librerias/librerias.R")

# -----------------------------------------------------------------------------
# Parámetros
# -----------------------------------------------------------------------------

periodo <- 5
periodo <- str_pad(periodo,2,"left","0")

# -----------------------------------------------------------------------------
# Lectura base de cobertura
# -----------------------------------------------------------------------------

ruta <- paste0("intermedios/04_cobertura/periodo_",periodo,
               "/cobertura_base_periodo_",periodo,".rds")
cobertura_base <- readRDS(ruta)

# -----------------------------------------------------------------------------
# Descriptivo periodo 
# -----------------------------------------------------------------------------

cobertura_base <- cobertura_base %>% 
  filter(periodo == as.numeric(periodo)) %>% 
  mutate(man_nloc = ifelse(zon == "999", "000", man_nloc),
         id_edif = paste0(pro, can, par, zon, sec, man_nloc, n_umce),
         id_viv = paste0(pro, can, par, zon, sec, man_nloc, n_umce, n_viv)) %>% 
  select(-c("tipovia", "tipoviaobs", "lote", "bloque", "patio", "piso", "casa", 
            "dpto", "parjefe", "numpers", "celular", "telefono", "finienc", 
            "ffinenc", "horini", "minini", "horfin", "minfin", "rzonal", "codenc",
            "cedcri", "check1", "check2", "check3", "check4", "check5", "check6",
            "check7", "check8", "check9", "check10", "xzonal", "xprovincia", 
            "xciudad", "xconglo", "xvivienda", "xhogar", "tomagps1", "latitud1", "longitud1",
            "codsup", "codcri", "cedenc", "cedsup", "nhogares", 
            "localidad", "informante" )) %>% 
  #filter(semana %in%  vec_sem) %>% 
  select(zonal, id_upm,pro,can,par,zon,sec,man_nloc,n_umce,n_viv,id_edif,id_viv, periodo, semana) 

# -----------------------------------------------------------------------------
# Muestra periodo 
# -----------------------------------------------------------------------------

if (periodo =="01"){
  
  muestra_per <- read_excel(paste0("productos/02_muestra_usm/periodo_",periodo,
                                   "/muestra_periodo_01_myc.xlsx"))
}else{
  muestra_per <- read_excel(paste0("productos/02_muestra_usm/periodo_",periodo,
                                   "/muestra_usm_myc.xlsx"))
}
muestra_per <- muestra_per %>% 
  mutate(numviv = str_pad(numviv, 4, "left", "0"), 
         man_nloc = ifelse(zona == "999", "000", manzana),
         id_edif = paste0(provin, canton, parroq, zona, sector, man_nloc, num_edif),
         id_viv = paste0(provin,canton,parroq,zona,sector,man_nloc,num_edif, numviv)) %>% 
  #filter(semana %in%  c(1,2)) %>% 
  select(zonal, id_upm, provin, canton, parroq,zona,sector,man_nloc,num_edif, numviv,id_edif,id_viv,
         periodo, semana) %>% 
  rename(pro = provin, can = canton, par = parroq, zon = zona, sec = sector,
         n_umce = num_edif)

# -----------------------------------------------------------------------------
# Validación muestra periodo  con cobertura periodo 
# -----------------------------------------------------------------------------

revision <- muestra_per %>% 
  full_join(cobertura_base , by = "id_viv") %>% 
  mutate(control = ifelse(is.na(pro.x) | is.na(pro.y),1,0)) %>% 
  filter(control == 1) 

viv_muestra_no_cober <- muestra_per %>% filter(!id_viv %in% cobertura_base$id_viv)

viv_cober_no_muestra <- cobertura_base %>% filter(!id_viv %in% muestra_per$id_viv)

dim(revision)[1] == dim(viv_muestra_no_cober)[1] + dim(viv_cober_no_muestra)[1]

table(viv_cober_no_muestra$semana)
table(viv_muestra_no_cober$semana)

# -----------------------------------------------------------------------------
# Exportando
# -----------------------------------------------------------------------------

ruta_exp <- paste0("intermedios/04_cobertura/periodo_",periodo,"/novedades")

wb <- createWorkbook("Inconsistencias base cobertura DIES")
addWorksheet(wb, "viv_muestra_no_cober")
addWorksheet(wb, "viv_cober_no_muestra")

writeData(wb, sheet = "viv_muestra_no_cober", viv_muestra_no_cober)
writeData(wb, sheet = "viv_cober_no_muestra", viv_cober_no_muestra)
saveWorkbook(wb, paste0(ruta_exp, paste0("/nov_cob_per_",periodo,".xlsx")), overwrite = T)


