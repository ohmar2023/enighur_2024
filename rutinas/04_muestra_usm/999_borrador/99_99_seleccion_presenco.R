rm(list = ls())

{
  library(tidyverse)
  library(sf)
  library(inborutils)
  library(rio)
  library(tidyverse)
  library(janitor)
  library(readxl)
  library(openxlsx) 
  library(rio)
  library(readr)
  library(sampling)
}

# -----------------------------------------------------------------------------
# UPMs no levantadas
# -----------------------------------------------------------------------------

upm_no_lev <- read_excel("intermedios/03_enlistamiento/01_concistencia/periodo_02/incon_man_sec_upm_periodo_02.xlsx", 
                         sheet = "upm_no_enlistadas")

# -----------------------------------------------------------------------------
# Importando muestra
# -----------------------------------------------------------------------------

muestra <- import("productos/02_muestra_upm/muestra_upm_man_sec_fondo_rot_004.xlsx")

# -----------------------------------------------------------------------------
# Importando marco de viv precenso-2022
# -----------------------------------------------------------------------------

marco_upm_no_lev <- readRDS("insumos/99_marco_viv_precenso/marco_viv_ocu_nap.rds") %>% 
  filter(id_upm %in% upm_no_lev$id_upm) %>% 
  rename(man_nloc = manz_loc, n_umce = n_edif,
         dpa_pro = nprovincia, dpa_can = ncanton, dpa_parr = nparroq,
         zonal = nregional, tot_hbt = n_hbt) %>% 
  mutate( id_viv = paste0(pro, can, par, zon, sec, man_nloc, n_umce, n_viv),
          zonal = ifelse(zonal == "PLANTA CENTRAL", "ADM. C. CAMPO", zonal),
          celular = "000",
          convencional = "000") %>% 
  left_join(muestra %>% filter(!duplicated(id_upm)) %>% 
              select(id_upm,semana_nueva,periodo_nuevo),by = "id_upm")

#names(marco_viv_ocu_nap)

# -----------------------------------------------------------------------------
# Marco de UPMs no levantadas
# -----------------------------------------------------------------------------

# marco_upm_no_lev <- marco_viv_ocu_nap %>% 
#   filter(id_upm %in% upm_no_lev$id_upm) %>% 
#   rename(man_nloc = manz_loc, n_umce = n_edif,
#          dpa_pro = nprovincia, dpa_can = ncanton, dpa_parr = nparroq,
#          zonal = nregional, tot_hbt = n_hbt) %>% 
# mutate( id_viv = paste0(pro, can, par, zon, sec, man_nloc, n_umce, n_viv),
#         zonal = ifelse(zonal == "PLANTA CENTRAL", "ADM. C. CAMPO", zonal),
#         celular = "000",
#         convencional = "000") %>% 
#   left_join(muestra %>% filter(!duplicated(id_upm)) %>% 
#               select(id_upm,semana_nueva,periodo_nuevo),by = "id_upm")
  
# -----------------------------------------------------------------------------
# Selección de 12 viv en cada UPM 
# -----------------------------------------------------------------------------

library(sampling)
set.seed(20011051)

muestra_upm_no_lev <- marco_upm_no_lev %>% 
  group_by(id_upm) %>% 
  mutate(freq = n(),
         n = ifelse(freq < 12, freq, 12),
         pik = inclusionprobabilities(freq, unique(n)),
         sel = UPsystematic(pik, eps=1e-6)) %>% 
  filter(sel == 1) %>% 
  ungroup() %>% 
  arrange(id_viv) %>% 
  group_by(id_upm) %>% 
  mutate(no_orden = row_number()) %>% 
  ungroup() %>% 
  mutate(dom = substr(estrato,1,2),
         dom = ifelse(dom == 31, pro, dom),
         n_dom = case_when( dom == 30 ~ "CUENCA",
                            dom == 32 ~ "LOJA_c",
                            dom == 33 ~ "QUITO",
                            dom == 34  ~ "AMBATO",
                            dom == 40  ~ "MACHALA",
                            dom == 41  ~ "ESMERALDAS_c",
                            dom == 42 ~ "GUAYAQUIL",
                            dom == 43 ~ "MANTA",
                            dom == 44  ~ "SANTO DOMINGO",
                            TRUE ~ dpa_pro),
         regional = case_when( zonal == "ADM. C. CAMPO"~ 9,
                               zonal == "LITORAL" ~ 8,
                               zonal == "SUR" ~ 6, 
                               zonal == "CENTRO" ~ 3),
         n_pm = ifelse(substr(n_pm,1,3) == "S/N" | is.na(n_pm),"S-N",n_pm),
         jefehoga = paste0(primernjh," ",segundonjh," ",primerajh," ",segudonjh),
         jefehoga = ifelse(jefehoga == "NA NA NA NA","Sin Nombre",jefehoga),
         jefehoga = gsub(jefehoga,pattern = " NA | NA|NA |NN",replacement = " "),
         jefehoga = gsub(jefehoga,pattern = "  ",replacement = ""),
         n_via_p = gsub(n_via_p,pattern = "-",replacement = " "))

# -----------------------------------------------------------------------------
# Formato MyC
# -----------------------------------------------------------------------------

muestra_myc_upm_no_lev <- muestra_upm_no_lev %>% select(no_orden = no_orden,
                                                     id_upm,
                                                     provin = pro,
                                                     canton = can,
                                                     parroq = par,
                                                     zona = zon,
                                                     sector = sec,
                                                     manzana = man,
                                                     num_edif = n_umce,
                                                     numviv = n_viv,
                                                     piso = piso_n,
                                                     calle = n_via_p,
                                                     numnum = n_pm,#NO. (PLACA MUNICIPAL)
                                                     vivienda = no_orden, # revisar
                                                     area = area,
                                                     estrato = estrato,
                                                     regional = regional, # revisar
                                                     zonal = zonal,
                                                     nprovin = dpa_pro,
                                                     ncanton = dpa_can,
                                                     nparroq = dpa_parr,
                                                     jefehoga = jefehoga,
                                                     numper = tot_hbt,
                                                     pluscode = pluscodes,
                                                     dominio = dom,
                                                     ndominio = n_dom, #las ciu estan LOJA_c
                                                     periodo = periodo_nuevo, # <- ojo
                                                     semana = semana_nueva,# <- ojo
                                                     telefono1 = celular,
                                                     telefono2 = convencional) 

# -----------------------------------------------------------------------------
# Exportando
# -----------------------------------------------------------------------------

export(muestra_myc_upm_no_lev,"muestra_myc_upm_no_lev.xlsx")
