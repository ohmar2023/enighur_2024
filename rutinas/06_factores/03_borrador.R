

#rm(list = ls())

#source("rutinas/99_librerias/librerias.R")

# ------------------------------------------------------------------------------
# Juntando todos los marcos de viv elaborados previos a la selección de la muestra
# De esta base se selecciónó en cada periodo la muestra
# ------------------------------------------------------------------------------

marco_viv_muestra_acum_enlist <- NULL

for (i in c(1:7)){
  periodo <- i
  periodo <- str_pad (periodo, 2, "left", "0")
  ruta <- paste0("productos/02_muestra_usm/",
                 "periodo_", periodo,"/marco_viv_muestra.rds")
  aux <- import(ruta)
  marco_viv_muestra_acum_enlist <- rbind(marco_viv_muestra_acum_enlist, aux)
}

# ------------------------------------------------------------------------------
# Juntando todas las bases de actualziación cartografica tal como me llegan 
# Esta base no es procesada ni filtrada nada
# ------------------------------------------------------------------------------

base_enlist_acum <- NULL

for (i in c(1:7)){
  periodo <- i
  periodo <- str_pad(periodo, 2, "left", "0")
  ruta <- paste0("intermedios/03_enlistamiento/01_concistencia/",
                 "periodo_", periodo,"/base.rds")
  aux <- import(ruta)
  base_enlist_acum <- rbind(base_enlist_acum, aux)
}

n_variables <- c("id_upm", "pro", "can", "par", "zon", "sec", "man", "n_umce", 
                 "n_viv", "piso_n", "id_viv", "n_via_p", "n_pm","area", "estrato", 
                 "zonal", "dpa_pro", "dpa_can", "dpa_parr", "jefe_hogar", "tot_hbt", 
                 "pluscodes", "periodo_nuevo", "semana_nueva",  "celular", "convencional",
                 "id_edif", "id_viv", "man_nloc","estrato","c_ocup")

base_enlist_acum <- base_enlist_acum %>% 
  mutate(jefe_hogar = paste0(primernjh," ",segundonjh," ",primerajh," ",segudonjh),
         jefe_hogar = ifelse(jefe_hogar == "NA NA NA NA","Sin Nombre",jefe_hogar),
         jefe_hogar = gsub(jefe_hogar,pattern = " NA | NA|NA |NN",replacement = " "),
         jefe_hogar = gsub(jefe_hogar,pattern = "  ",replacement = ""),
         n_via_p = case_when( is.na(n_via_s) ~ n_via_p,
                              TRUE ~ paste0(n_via_p," Y ", n_via_s))) %>% 
  select(all_of(n_variables))

# ------------------------------------------------------------------------------
# Entre las dos bases previas me quedo con una sola que contegna todo la info
# del levantamiento de actualización
# Hago este mix de bases pirque hay UPM que no son actualziadas y se las saca del precenso
# Entonces
# ------------------------------------------------------------------------------

aux_1 <- marco_viv_muestra_acum_enlist %>% 
  filter(!id_upm %in% base_enlist_acum$id_upm) 

# dim(aux_1)
# dim(base_enlist_acum)
# n_distinct(aux_1$id_upm)
# table(aux_1$c_ocup)

 base_enlist_acum <- rbind(base_enlist_acum, aux_1)

  
# n_distinct(base_enlist_acum$id_upm)

# ------------------------------------------------------------------------------
# Entre las dos bases previas me quedo con una sola que contegna todo la info
# del levantamiento de actualización
# Hago este mix de bases pirque hay UPM que no son actualziadas y se las saca del precenso
# Entonces
# ------------------------------------------------------------------------------

base_act_acum_1 <- base_enlist_acum %>% 
  filter(grepl(c_ocup, pattern = "ocupada con|base ocupada|rechazo")) %>% 
  group_by(id_upm) %>% 
  mutate(Ni_enlist = n()) %>% #Cantidad de viviendas por upm
  ungroup() %>% 
  group_by(estrato) %>% 
  mutate(Nh_enlis = n()) %>% # Cantidad de viviendas por estrato
  ungroup() %>% 
  group_by(id_upm, pro, area, estrato, Ni_enlist, Nh_enlis) %>% 
  summarise() %>% 
  ungroup() 
  
# colSums(is.na(base_act_acum_1))
# n_distinct(base_act_acum_1$id_upm)
# unique(base_act_acum_1$c_ocup)
# #rio::export(base_act_acum_1, "base_act_acum_1.xlsx")
# 
# 
# v_c_ocup <- unique(base_enlist_acum$c_ocup)
# v_c_ocup[grepl(v_c_ocup, pattern = "ocupada con|base ocupada|rechazo")]
# unique()
