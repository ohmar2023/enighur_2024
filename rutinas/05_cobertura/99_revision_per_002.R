
rm(list = ls())

source("rutinas/99_librerias/librerias.R")

# -----------------------------------------------------------------------------
# Parámetros
# -----------------------------------------------------------------------------

p <- 2

# -----------------------------------------------------------------------------
# Lectura base de cobertura
# -----------------------------------------------------------------------------

ruta <- paste0("intermedios/04_cobertura/base_cobertura/cobertura_base.rds")
cobertura_base <- readRDS(ruta)

cobertura_base <- cobertura_base %>% 
  filter(periodo == p) %>% 
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

muestra_per <- readRDS("productos/02_muestra_usm/periodo_02/muestra_usm_inter.rds") %>% 
  filter(sel == 1) %>% 
  mutate(man_nloc = ifelse(!is.na(man), man, "000"),
         id_edif = paste0(pro, can, par, zon, sec, man_nloc, n_umce),
         id_viv = paste0(pro, can, par, zon, sec, man_nloc, n_umce, n_viv)) %>% 
  select(zonal,id_upm,pro,can, par, zon, sec, man_nloc, n_umce, n_viv,id_edif,
         id_viv, periodo = periodo_nuevo, semana = semana_nueva)

# -----------------------------------------------------------------------------
# Validación muestra periodo 1 con cobertura periodo 1
# -----------------------------------------------------------------------------

revision <- muestra_per %>% 
  #filter(id_upm == "090150181601") %>% 
  #filter(pro == a) %>% 
  full_join(cobertura_base , by = "id_viv") %>% 
  mutate(control = ifelse(is.na(pro.x) | is.na(pro.y),1,0)) %>% 
  filter(control == 1) 

viv_muestra_no_cober <- muestra_per %>% filter(!id_viv %in% cobertura_base$id_viv)

viv_cober_no_muestra <- cobertura_base %>% filter(!id_viv %in% muestra_per$id_viv)

dim(revision)[1] == dim(viv_muestra_no_cober)[1] + dim(viv_cober_no_muestra)[1]

table(viv_cober_no_muestra$semana)
table(viv_muestra_no_cober$semana)



table(nchar(muestra_per$id_viv))
table(nchar(cobertura_base$id_viv))

muestra_per %>% filter(nchar(id_viv) == 21) %>% View()
