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
# Lectura nombres prov, can, parr
# -----------------------------------------------------------------------------

codif_dpa <- read_excel("insumos/99_nombres_parroquias/CODIFICACIÓN_DPA_PAIS_2022_fin.xlsx") %>% 
  clean_names() %>% 
  rename(par = cod_parr_pais) %>% 
  select(c(1:4)) %>% 
  filter(!duplicated(par)) %>% 
  mutate(par = str_pad(par,6, "left", "0"),
         pro = substr(par,1,2),
         can = substr(par,3,4),
         par = substr(par,5,6))

# -----------------------------------------------------------------------------
# 
# -----------------------------------------------------------------------------

muestra <- import("muestra_upm_man_sec_fondo_rot_004.xlsx")
marco_upm <- readRDS("D:/Omar/INEC/GIT/enighur_2024/insumos/02_muestra_upm/marco/marco_upm.rds")



# -----------------------------------------------------------------------------
# Seleccion de viviendas
# -----------------------------------------------------------------------------

ocupada %>% group_by(id_upm) %>% summarise(n()) %>% View()

mutate(freq = n(),
       
       pik = inclusionprobabilities(freq, unique(n)),
       
       sel = UPsystematic(pik, eps=1e-6))


ocupada_muestra <- ocupada %>% 
  left_join(codif_dpa,by = c("pro","can","par")) %>% 
  left_join(select(marco_upm,estrato,area,id_upm),by = "id_upm") %>% 
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
         n_dom = case_when( dom == 30 ~ "CUENCA",
                            dom == 32 ~ "LOJA",
                            dom == 33 ~ "QUITO",
                            dom == 34  ~ "AMBATO",
                            dom == 40  ~ "MACHALA",
                            dom == 41  ~ "ESMERALDAS",
                            dom == 42 ~ "GUAYAQUIL",
                            dom == 43 ~ "MANTA",
                            dom == 44  ~ "SANTO DOMINGO",
                            TRUE ~ dpa_pro),
         regional = case_when( zonal == "ADM. C. CAMPO"~ "9",
                               zonal == "LITORAL" ~ "8",
                               zonal == "SUR" ~ "6", 
                               zonal == "CENTRO" ~ "3"),
         jor = 1,
         sem = 1)
  
  

ocupada_muestra %>% group_by(n_dom) %>% summarise(n()) %>% View()
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
                                          jefehoga = primernjh,
                                          numper = tot_hbt,
                                          pluscode = pluscodes,
                                          dominio = dom,
                                          ndominio = n_dom,
                                          periodo = jor, # <- ojo
                                          semana = sem,# <- ojo
                                          telefono1 = celular,
                                          telefono2 = convencional) 
export(muestra_myc, "muestra_myc.xlsx")

muestra_myc %>% group_by(id_upm) %>% summarise(n()) %>% View()

