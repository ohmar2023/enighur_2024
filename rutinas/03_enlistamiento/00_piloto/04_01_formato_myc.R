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

upm_inc <- read_excel("insumos/03_enlistamiento/2024_11_28/incon_man_sec_upm_202412REvisadoCartografia.xlsx",
                      sheet = "upm_no_enlistadas")
# -----------------------------------------------------------------------------
# 
# -----------------------------------------------------------------------------

muestra <- import("muestra_upm_man_sec_fondo_rot_004.xlsx")
marco_upm <- readRDS("D:/Omar/INEC/GIT/enighur_2024/insumos/02_muestra_upm/marco/marco_upm.rds")

# -----------------------------------------------------------------------------
# Seleccion de viviendas
# -----------------------------------------------------------------------------

ocupada %>% group_by(id_upm) %>% summarise(n()) %>% View()

ocupada_muestra <- ocupada %>% filter(!id_upm %in% upm_inc$id_upm) %>% 
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
         n_pm = ifelse(substr(n_pm,1,3) == "S/N" | is.na(n_pm),"S/N",n_pm),
         jefehoga = paste0(primernjh," ",segundonjh," ",primerajh," ",segudonjh),
         jefehoga = ifelse(jefehoga == "NA NA NA NA","Sin Nombre",jefehoga),
         jefehoga = gsub(jefehoga,pattern = " NA | NA|NA |NN",replacement = " "),
         jefehoga = gsub(jefehoga,pattern = "  ",replacement = ""),
         jor = 1,
         sem = 1)

# -----------------------------------------------------------------------------
# Control de variables para la creación del MyC. Deben dar -> cero
# -----------------------------------------------------------------------------

ocupada_muestra %>% filter(is.na(n_pm)) %>% dim()
ocupada_muestra %>% filter(is.na(pluscodes)) %>% dim()

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
                                          jefehoga = jefehoga,
                                          numper = tot_hbt,
                                          pluscode = pluscodes,
                                          dominio = dom,
                                          ndominio = n_dom, #las ciu estan LOJA_c
                                          periodo = jor, # <- ojo
                                          semana = sem,# <- ojo
                                          telefono1 = celular,
                                          telefono2 = convencional) 

export(muestra_myc, "productos/02_muestra_usm/muestra_myc.xlsx")

