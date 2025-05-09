
source("rutinas/99_librerias/librerias.R")

rm(list = ls())

# -----------------------------------------------------------------------------
# Parámetros
# -----------------------------------------------------------------------------

p <- 1

# -----------------------------------------------------------------------------
# Lectura base de cobertura
# -----------------------------------------------------------------------------

ruta <- paste0("intermedios/04_cobertura/base_cobertura/cobertura_base.rds")
cobertura_base <- readRDS(ruta)

# -----------------------------------------------------------------------------
# Descriptivo periodo 1
# -----------------------------------------------------------------------------

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

cobertura_base_sem_3 <- cobertura_base %>% filter(semana == 3)


# -----------------------------------------------------------------------------
# Muestra semana 3
# -----------------------------------------------------------------------------

# muestra_1_sem_3 <- read_excel("productos/02_muestra_usm/periodo_01/muestra_myc.xlsx") %>%
#   mutate(man_nloc = ifelse(zona == "999", "000", manzana),
#          id_edif = paste0(provin, canton, parroq, zona, sector, man_nloc, num_edif),
#          id_viv = paste0(provin,canton,parroq,zona,sector,man_nloc,num_edif, numviv)) %>%
#   filter(semana %in% c(3)) %>%
#   select(zonal, id_upm, provin, canton, parroq,zona,sector,man_nloc,num_edif, numviv,id_edif,id_viv,
#          periodo, semana) %>%
#   rename(pro = provin, can = canton, par = parroq, zon = zona, sec = sector,
#          n_umce = num_edif)

muestra_2_sem_3 <- read_excel("productos/02_muestra_usm/periodo_01/muestra_myc_semana_3.xlsx") %>% 
  mutate(man_nloc = ifelse(zona == "999", "000", manzana),
         id_edif = paste0(provin, canton, parroq, zona, sector, man_nloc, num_edif),
         id_viv = paste0(provin,canton,parroq,zona,sector,man_nloc,num_edif, numviv)) %>% 
  select(zonal, id_upm, provin, canton, parroq,zona,sector,man_nloc,num_edif, numviv,id_edif,id_viv,
         periodo, semana) %>% 
  rename(pro = provin, can = canton, par = parroq, zon = zona, sec = sector,
         n_umce = num_edif)

revision_m_2_s_3 <- muestra_2_sem_3 %>% 
  full_join(cobertura_base_sem_3 , by = "id_viv") %>% 
  mutate(control = ifelse(is.na(pro.x) | is.na(pro.y),1,0)) %>% 
  filter(control == 0) 

viv_muestra_no_cober <- muestra_2_sem_3 %>% filter(!id_viv %in% cobertura_base_sem_3$id_viv)
viv_cober_no_muestra <- cobertura_base_sem_3 %>% filter(!id_viv %in% muestra_2_sem_3$id_viv)

export(revision_m_2_s_3,"revision_m_2_s_3.xlsx")

viv_muestra_no_cober %>% group_by(id_upm) %>% summarise(no_cober = n()) %>% 
  left_join(
viv_cober_no_muestra %>% group_by(id_upm) %>% summarise(n_no_mues = n()), by = "id_upm") %>% export("Upm_montadas.xlsx")





















