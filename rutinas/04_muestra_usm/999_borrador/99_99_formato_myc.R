
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
# Lectura de base-DICA
# -----------------------------------------------------------------------------

base <- readRDS("intermedios/03_enlistamiento/01_concistencia/periodo_02/base.rds")

# -----------------------------------------------------------------------------
# Vivendas ocupadas
# -----------------------------------------------------------------------------

ocupada <- base %>% 
  filter(grepl(c_ocup,pattern = "ocupada con|rechazo")) %>% 
  filter(periodo_nuevo == 2) 

# -----------------------------------------------------------------------------
# Seleccion de viviendas
# -----------------------------------------------------------------------------

library(sampling)
set.seed(20241224)
ocupada_muestra <- ocupada %>% 
  #filter(!id_upm %in% upm_inc$id_upm) %>% 
  #left_join(codif_dpa,by = c("pro","can","par")) %>% 
  #left_join(select(marco_upm,estrato,area,id_upm),by = "id_upm") %>% 
  arrange(id_viv) %>% 
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
         celular = ifelse(is.na(celular),"000",celular),
         convencional = ifelse(is.na(convencional),"000",convencional),
         n_via_p = gsub(n_via_p,pattern = "-",replacement = " "))

# -----------------------------------------------------------------------------
# Control de variables para la creación del MyC. Deben dar -> cero
# -----------------------------------------------------------------------------

ocupada_muestra %>% filter(semana_nueva == 3) %>% filter(is.na(n_pm)) %>% dim()
ocupada_muestra %>% filter(semana_nueva == 3) %>% filter(is.na(pluscodes)) %>% dim()
ocupada_muestra %>% filter(semana_nueva == 3 & !duplicated(id_edif)) %>% 
  filter(is.na(pluscodes)) %>% dim()

#ocupada_muestra %>% group_by(n_dom) %>% summarise(n()) %>% View()
# -----------------------------------------------------------------------------
# 
# -----------------------------------------------------------------------------

muestra_myc <- ocupada_muestra %>% select(no_orden = no_orden,
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
                                          jefehoga = jefe_hogar,
                                          numper = tot_hbt,
                                          pluscode = pluscodes,
                                          dominio = dom,
                                          ndominio = n_dom, #las ciu estan LOJA_c
                                          periodo = periodo_nuevo, # <- ojo
                                          semana = semana_nueva,# <- ojo
                                          telefono1 = celular,
                                          telefono2 = convencional) 

muestra_myc <- muestra_myc %>% 
  mutate(ndominio = gsub(ndominio,pattern = "_",replacement = " "),
         calle = gsub(calle,pattern = "°",replacement = " "),
         calle = gsub(calle,pattern = "\\(|\\)",replacement = ""),
         calle = gsub(calle,pattern = "\\,|\\.",replacement = ""),
         numnum = gsub(numnum,pattern = "/" ,replacement = ""),
         nparroq = gsub(nparroq,pattern = "EL PARAISO LA 14",
                        replacement = "EL PARAISO LA CATORCE"))



# -----------------------------------------------------------------------------
# 
# -----------------------------------------------------------------------------

export(muestra_myc , "productos/02_muestra_usm/periodo_02/muestra_myc_per_2.xlsx")




















# -----------------------------------------------------------------------------
# borrador
# -----------------------------------------------------------------------------

p <- muestra_myc %>% filter(!(duplicated(id_upm)))
table(p$semana)
table(p$periodo)

muestra_myc %>% mutate(n_carac = nchar(piso),
                       pri = ifelse(n_carac==10,substr(piso, 9, 10),"x"),
                       seg = ifelse(n_carac==10,substr(piso, 6,  7),"x"),
                       
                       pri = gsub(pri,pattern = "0", replacement = ""),
                       seg = gsub(seg,pattern = "0", replacement = ""),
                       
                       piso_nuevo = ifelse(pri=="x",piso,paste0(pri,"-",seg)),
                       
                       piso = piso_nuevo
                       ) %>% 
  select(-all_of(c("n_carac", "pri", "seg", "piso_nuevo"))) %>% View()


