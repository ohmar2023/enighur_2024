# ------------------------------------------------------------------------------
# El objetivo de este script es generar la información que proviene del proceso
# de actualización cartográfica.
# La base resultante es base_act_acum_1.
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Juntando todos los marcos de viv elaborados previos a la selección de la muestra
# De esta base, en su momento,  se selecciónó en cada periodo la muestra de viviendas
# ------------------------------------------------------------------------------

periodo_ref <- 11
marco_viv_muestra_acum_enlist <- NULL

for (i in c(1:periodo_ref)){
  periodo <- i
  periodo <- str_pad (periodo, 2, "left", "0")
  ruta <- paste0("productos/02_muestra_usm/",
                 "periodo_", periodo,"/marco_viv_muestra.rds")
  aux <- import(ruta)
  marco_viv_muestra_acum_enlist <- rbind(marco_viv_muestra_acum_enlist, aux)
}

# ------------------------------------------------------------------------------
# Juntando todas las bases de actualziación cartografica.
# Esta base no es procesada ni filtrada nada, bases sin modificaciones.
# ------------------------------------------------------------------------------

base_enlist_acum <- NULL

for (i in c(1:periodo_ref)){
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
# Entre las dos bases previas me quedo con una sola que contenga toda la info
# del levantamiento de actualización.
# Hago este mix de bases porque hay UPM que no son actualizadas y se las saca del precenso
# Entonces al juntarlas ya tengo disponible todo lo que se junta para actualizar.

# aux_1: Contiene la información del precenso de aquellas UPM que no fueron 
#       actualzadas. Se pudo haber hecho mas simple, identificando dichas UPM
#       y luego buscandolas en el precenso.

# base_enlist_acum: Tiene la info de las upm actualizadas + las no actualizadas.
#                   De esta manera tenemos la base madre que tiene los totales
#                   que se requieren para los factores.
# ------------------------------------------------------------------------------

aux_1 <- marco_viv_muestra_acum_enlist %>% 
  filter(!id_upm %in% base_enlist_acum$id_upm) 

# ------------------------------------------------------------------------------
# base_enlist_acum: Tiene la info de las upm actualizadas + las no actualizadas.
# ------------------------------------------------------------------------------

 base_enlist_acum <- rbind(base_enlist_acum, aux_1)

# ------------------------------------------------------------------------------
# base_act_acum_1: Contiene las variables necesarias para los factores:
#                  Ni_enlist y Nh_enlis.                  
# ------------------------------------------------------------------------------

base_act_acum <- base_enlist_acum %>% 
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
  
# ------------------------------------------------------------------------------
# Exportando                  
# ------------------------------------------------------------------------------

ruta <- "intermedios/05_factores/01_teoricos_ajuste_cobertura/"
rio::export(base_act_acum, paste0(ruta, "base_act_acum.rds"))








