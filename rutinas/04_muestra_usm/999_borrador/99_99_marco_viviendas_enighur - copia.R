
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
# Parametros:
# -----------------------------------------------------------------------------

periodo = 2
fecha <- "2025_01_16" # se usa como semilla, ya le transforma en numeric

# -----------------------------------------------------------------------------
# Lectura de base-DICA : Considera el periodo
# -----------------------------------------------------------------------------

periodo <- str_pad(periodo, 2, "left", "0")
ruta <- paste0("intermedios/03_enlistamiento/01_concistencia/","periodo_",periodo)

base_ocupada <- readRDS(paste0(ruta,"/base.rds")) %>% 
  mutate(jefe_hogar = paste0(primernjh," ",segundonjh," ",primerajh," ",segudonjh),
         jefe_hogar = ifelse(jefe_hogar == "NA NA NA NA","Sin Nombre",jefe_hogar),
         jefe_hogar = gsub(jefe_hogar,pattern = " NA | NA|NA |NN",replacement = " "),
         jefe_hogar = gsub(jefe_hogar,pattern = "  ",replacement = "")) %>% 
  filter(grepl(c_ocup,pattern = "ocupada con|rechazo")) %>% 
  filter(periodo_nuevo == as.numeric(periodo)) 

# -----------------------------------------------------------------------------
# Lectura supermanzanas
# -----------------------------------------------------------------------------

upm_super_man <- read_excel("insumos/99_supermanzanas/upm_enighur_partir.xlsx")
particion_manzanas_li_60 <- read_rds("insumos/99_supermanzanas/particion_manzanas_li_60.rds")

# -----------------------------------------------------------------------------
# Importando muestra
# -----------------------------------------------------------------------------

muestra <- import("productos/02_muestra_upm/muestra_upm_man_sec_fondo_rot_004.xlsx")

# -----------------------------------------------------------------------------
# Lectura UPM´s sin enlistar
# -----------------------------------------------------------------------------

upm_no_lev <- read_excel(paste0(ruta,"/incon_man_sec_upm_periodo_",periodo,".xlsx"), 
                         sheet = "upm_no_enlistadas")

# -----------------------------------------------------------------------------
# Importando marco de viv precenso-2022
# Marco de viviendas de UPM no enlistadas
# -----------------------------------------------------------------------------

if (dim(upm_no_lev)[1]==0) 
{
  message("NO HAY UPM NO LEVANATADAS") 
  marco_upm_no_lev <- NULL
} else{
marco_upm_no_lev <- readRDS("insumos/99_marco_viv_precenso/marco_viv_ocu_nap.rds") %>% 
  filter(id_upm %in% upm_no_lev$id_upm) %>% 
  rename(man_nloc = manz_loc, n_umce = n_edif,
         dpa_pro = nprovincia, dpa_can = ncanton, dpa_parr = nparroq,
         zonal = nregional, tot_hbt = n_hbt) %>% 
  mutate( id_viv = paste0(pro, can, par, zon, sec, man_nloc, n_umce, n_viv),
          zonal = ifelse(zonal == "PLANTA CENTRAL", "ADM. C. CAMPO", zonal),
          celular = "000",
          convencional = "000",
          c_ocup = "base ocupada") %>% 
  left_join(muestra %>% filter(!duplicated(id_upm)) %>% 
              select(id_upm,semana_nueva,periodo_nuevo),by = "id_upm")
}

# -----------------------------------------------------------------------------
# Marco de viviendas super manzanas: Con el grupo ya sabriamos que edificio
# corresponde a que upm. Sin embargo, existen casos en los que se crea una nueva
# numeración para un nuevo edificio que se ha construido, por lo que se debe
# asignar a una UPM este edificio. Cuando detectan esto, lo ponen en el orden
# que lo encuentran,e.d, 1,2,3,nuevo,4,5 el nuevo seria el 6 y en la base 
# se visualiza asi 1,2,3,6,4,5. Lo que resta es asignar a una UPM este edif 6
# para lo que se decide en los for que se programan a continuación y lo que que 
# se busca es que esté en la UPM correcta o en su defecto en la mas cercana. 

# Falta programar el hecho de que gr1 sea cero. Falta automatizar si nos quedamos
# con gr_1, gr_2 o gr_3
# -----------------------------------------------------------------------------

marco_viv_superman <- base_ocupada %>% 
  mutate(id_conglomerado = substr(id_upm,1,10),
         n_upm_sel = substr(id_upm,12,12)) %>% 
  filter(id_conglomerado %in% upm_super_man$id_conglomerado) %>% 
  left_join(select(particion_manzanas_li_60,id_edif,grupo),by= "id_edif") 

table(marco_viv_superman$grupo,useNA = "ifany")

gr_1 <- c()
gr_2 <- c()

n = length(marco_viv_superman$grupo)
gr_1[1] = marco_viv_superman$grupo[1]
gr_2[n] = marco_viv_superman$grupo[n]

for (i in c(2:n)) {
  
  if(is.na(marco_viv_superman$grupo[i]) & 
     marco_viv_superman$man_sec_21[i] == marco_viv_superman$man_sec_21[i-1]){
    gr_1[i] = gr_1[i-1] }
  
  if(is.na(marco_viv_superman$grupo[i]) & 
     marco_viv_superman$man_sec_21[i] != marco_viv_superman$man_sec_21[i-1]){
    gr_1[i] = 0 }
  
  if(!is.na(marco_viv_superman$grupo[i])){ 
    gr_1[i] = marco_viv_superman$grupo[i] }
  
  j = n-i+1
  
  if(is.na(marco_viv_superman$grupo[j]) & marco_viv_superman$man_sec_21[j]== marco_viv_superman$man_sec_21[j+1]){
    gr_2[j] = gr_2[j+1]}
  
  if(is.na(marco_viv_superman$grupo[j]) & marco_viv_superman$man_sec_21[j]!= marco_viv_superman$man_sec_21[j+1]){
    gr_2[j] = 0 }
  
  if(!is.na(marco_viv_superman$grupo[j])){ 
    gr_2[j] = marco_viv_superman$grupo[j]
  }
}

#gr_1==gr_2

# cbind(marco_viv_superman,gr_1,gr_2) %>% select(man_sec_21,grupo,gr_1,gr_2) %>%
#  mutate(gr_3 = ifelse(gr_1 == gr_2, 1, 0)) %>%
#  View()

marco_viv_superman <- marco_viv_superman %>%  
  mutate(grupo = as.character(gr_1)) %>% 
  filter(grupo %in%  unique(n_upm_sel)) 

#grupo es igual a gr_1
# marco_viv_superman <- base_ocupada %>% 
#   mutate(id_conglomerado = substr(id_upm,1,10),
#          n_upm_sel = substr(id_upm,12,12)) %>% 
#   filter(id_conglomerado %in% upm_super_man$id_conglomerado) %>% 
#   left_join(select(upm_super_man,id_conglomerado,n_upm_con), by = "id_conglomerado") %>% 
#   arrange(id_viv,n_umce) %>% 
#   group_by(n_umce) %>% 
#   mutate(cant_viv = n()) %>% 
#   ungroup() %>% 
#   group_by(id_conglomerado) %>% 
#   mutate(cant_viv_tot = n(),
#          n_grupo = floor(cant_viv_tot/unique(n_upm_con)),
#          fila = row_number(),
#          grupo = cut(fila, 
#                      breaks = unique(n_upm_con), #numero de UPM en el conglomerado
#                      right = FALSE,
#                      labels = FALSE)) %>% 
#   filter(grupo == unique(n_upm_sel)) %>% # num de UPM seleccionada en la muestra
#   ungroup() 

# que pasa con n_upm_sel si hay mas de una UPM en el congl

# -----------------------------------------------------------------------------
# Seleccionando las mismas variables
# -----------------------------------------------------------------------------

n_variables <- c("id_upm", "pro", "can", "par", "zon", "sec", "man", "n_umce", 
                 "n_viv", "piso_n", "id_viv", "n_via_p", "n_pm","area", "estrato", 
                 "zonal", "dpa_pro", "dpa_can", "dpa_parr", "jefe_hogar", "tot_hbt", 
                 "pluscodes", "periodo_nuevo", "semana_nueva",  "celular", "convencional",
                 "id_edif", "id_viv", "man_nloc","estrato","c_ocup")

marco_viv_superman <- marco_viv_superman %>% select(all_of(n_variables))

base_ocupada <- base_ocupada  %>% 
  mutate(id_conglomerado = substr(id_upm,1,10)) %>% 
  filter(!id_conglomerado %in% upm_super_man$id_conglomerado) %>% 
  select(all_of(n_variables))

if (dim(upm_no_lev)[1] == 0) {
  marco_viv_muestra <- rbind(base_ocupada,marco_viv_superman)
} else{
  marco_upm_no_lev <- marco_upm_no_lev %>% select(all_of(n_variables))
  marco_viv_muestra <- rbind(base_ocupada,marco_viv_superman,marco_upm_no_lev)
}

# -----------------------------------------------------------------------------
# Seleccion de viviendas
# -----------------------------------------------------------------------------

library(sampling)

set.seed(20241226) #Periodo 2

#set.seed(as.numeric(gsub(fecha,pattern = "_",replacement = ""))) 

muestra_usm_inter <- marco_viv_muestra %>% 
  arrange(id_viv) %>% 
  group_by(id_upm) %>% 
  mutate(freq = n(),
         n = ifelse(freq < 12, freq, 12),
         pik = inclusionprobabilities(freq, unique(n)),
         sel = UPsystematic(pik, eps=1e-6)) %>% 
  #filter(sel == 1) %>% 
  # ungroup() %>% 
  # arrange(id_viv) %>% 
  # group_by(id_upm) %>% 
  # mutate(no_orden = row_number()) %>% 
  # ungroup() %>% 
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

muestra_usm <- muestra_usm_inter %>% filter(sel == 1) %>% 
  arrange(id_viv) %>% 
  group_by(id_upm) %>% 
  mutate(no_orden = row_number()) %>% 
  ungroup() 
  
aux <- muestra_usm %>% filter(!duplicated(id_upm))
table(aux$semana_nueva, useNA = "ifany")
table(aux$periodo_nuevo, useNA = "ifany")

# -----------------------------------------------------------------------------
#  Formato MyC : Base se carga al sistema para generar los MyC
# -----------------------------------------------------------------------------

muestra_usm_myc <- muestra_usm %>% select(no_orden = no_orden,
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

muestra_usm_myc <- muestra_usm_myc %>% 
  mutate(ndominio = gsub(ndominio,pattern = "_",replacement = " "),
         calle = gsub(calle,pattern = "°",replacement = " "),
         calle = gsub(calle,pattern = "\\(|\\)",replacement = ""),
         calle = gsub(calle,pattern = "\\,|\\.",replacement = ""),
         numnum = gsub(numnum,pattern = "/" ,replacement = ""),
         nparroq = gsub(nparroq,pattern = "EL PARAISO LA 14",
                        replacement = "EL PARAISO LA CATORCE"))

muestra_usm_myc <- muestra_usm_myc %>% 
  mutate(calle = ifelse(is.na(calle) | calle=="","S-N", calle),
         numnum = ifelse(is.na(numnum) | numnum=="","S-N",numnum),
         jefehoga = ifelse(is.na(jefehoga) | jefehoga=="","Sin Nombre",jefehoga),
         ncanton = gsub(ncanton,pattern = "\\(|\\)",replacement = ""))

sum(is.na(muestra_usm_myc$calle))
muestra_usm_myc$calle[615]
muestra_usm_myc$jefehoga[2753]

# -----------------------------------------------------------------------------
# Exportando resultados
# -----------------------------------------------------------------------------

export(muestra_usm_myc, # muestra seleccionada
       paste0("productos/02_muestra_usm/periodo_",periodo,"/muestra_usm_myc.xlsx"))

export(marco_viv_muestra, # base marco
       paste0("productos/02_muestra_usm/periodo_",periodo,"/marco_viv_muestra.rds"))

export(muestra_usm_inter, # base que tiene los pik
       paste0("productos/02_muestra_usm/periodo_",periodo,"/muestra_usm_inter.rds"))


muestra_usm[(is.na(muestra_usm))]

class(is.na(muestra_usm))

is.na(muestra_usm) %>% View()
