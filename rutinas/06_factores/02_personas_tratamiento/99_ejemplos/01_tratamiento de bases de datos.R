#
rm(list = ls())
#
library(rio)
library(tidyverse)
#
options(scipen = 999)
#
# ruta para las bases a leer
mes <- list.files("insumos/05_cobertura")
mes
# mes que queremos revisar
m <- max(length(mes))

fecha <- last(list.files(paste0("insumos/05_cobertura/", mes[m]), full.names = T))

# nombres de los archivos a leer
n_muestra <- list.files(paste0("productos/04_muestra_usm/", mes[m], "/"), pattern = "v2.xlsx", full.names = T) # antes "v2.0.xlsx"
n_vivienda <- last(list.files(fecha, pattern = "Vivienda_hogar", full.names = T))
n_caratula <- last(list.files(fecha, pattern = "Vivienda_dies", full.names = T))
n_personas <- last(list.files(fecha, pattern = "Personas", full.names = T))

# lectura y tratamiento de bases de datos

load("insumos/02_marcos/dpa_2022.RData")
rm(canton, parroquia)

# tratamiento base de muestra seleccionada
muestra <- import(n_muestra) %>%
  mutate(nregional = str_to_title(nregional),
         nprovincia = str_to_title(nprovincia),
         id_viv_car = paste0(provincia, canton, parroquia, zona, sector, manzana, num_edif, num_viv)) %>% 
  select(id_upm, panelm = panel, vivienda, estrato, area, mes,
         id_viv_car, panelviv, dominio, n_dominio, nregional, nprovincia, cod_cart)

# tratamiento base caratula
caratula <- import(n_caratula) |> 
  mutate(prov = str_pad(prov, 2, "left", "0"),
         cant = str_pad(cant, 2, "left", "0"),
         parr = str_pad(parr, 2, "left", "0"),
         zona = str_pad(zona, 3, "left", "0"),
         sector = str_pad(sector, 3, "left", "0"),
         manzana = str_pad(manzana, 3, "left", "0"),
         edificio = str_pad(edificio, 3, "left", "0"),
         nviv = str_pad(nviv, 3, "left", "0"),
         id_viv_car = paste0(prov, cant, parr, zona, sector, manzana, edificio, nviv),
         zonal = case_when(prov %in% c("08", "04", "10", "21", "17") ~ "CZ1",
                           prov %in% c("13", "23", "09", "12", "24", "20") ~ 'CZ2',
                           prov %in% c("02", "15", "22", "16", "05", "18", "06") ~ "CZ3",
                           prov %in% c("01", "03", "14", "07", "11", "19") ~ "CZ4",
                           T ~ "LOL"),
         id_upm = str_pad(id_conglomerado, 12, "left", "0"),
         panelm = str_pad(panelm, 3, "left", "0"),
         #vivienda =  str_pad(vivienda, 2, "left", "0"), hasta a25m03, luego viv_seguimiento
         viv_seguimiento =  str_pad(viv_seguimiento, 2, "left", "0"),
         hogar = str_pad(hogar, 2, "left", "0"),
         mes = mes[m],
         fv = as.Date.character(gsub("\\(.*", "", fechaentrev),format = "%Y-%m-%d")) %>%
  arrange(zonal, id_upm, panelm, viv_seguimiento, hogar) %>% 
  mutate(numpers = as.numeric(numpers)) %>% 
  # emparejamos la muestra para corregir los 3 primeros meses
  left_join(muestra %>% 
              select(id_viv_car, id_upm, vivienda, panelm, cod_cart),
            by = "id_viv_car", suffix = c("", "_c")) %>% 
  mutate(id_upm = case_when(prov == "20" ~ id_upm_c,
                              T ~ id_upm),
         viv_seguimiento = case_when(prov == "20" ~ vivienda_c,
                            T ~ viv_seguimiento),
         panelm = case_when(prov == "20" ~ panelm_c,
                            T ~ panelm)) %>% 
  select(zonal, pro = prov, id_upm, panelm, vivienda = viv_seguimiento, #panelm = panelm_c, vivienda = vivienda_c,
         hogar, fv, mes, cod_cart,  resultado_entrevista = resumen, res_ent_otros = resumen_1, numpers,
         id_viv_car, nom_jefehogar, fonoconvencional, fonocelular, piso, nummunicipio, calle)

# correr antes del select
n_distinct(paste0(caratula$id_viv_car))
n_distinct(paste0(caratula$id_viv_car, caratula$hogar))
sum(caratula$viv_seguimiento == caratula$vivienda_c)
sum(caratula$panelm == caratula$panelm_c)

#total
colSums(is.na(caratula))
table(caratula$resultado_entrevista, caratula$res_ent_otros, useNA = "ifany")

# tratamiento base personas (por separado, nac galapagos
personas_20 <- import(n_personas) %>% 
  # filtramos galapagos
  filter(prov == "20") %>% 
  mutate(viv_seguimiento =  str_pad(viv_seguimiento, 2, "left", "0"),
         panelm = str_pad(panelm, 3, "left", "0"),
         id_viv_car = paste0(str_pad(prov, 2, "left", "0"),
                             str_pad(cant, 2, "left", "0"),
                             str_pad(parr, 2, "left", "0"),
                             str_pad(zona, 3, "left", "0"),
                             str_pad(sector, 3, "left", "0"),
                             str_pad(manz, 3, "left", "0"),
                             str_pad(edif, 3, "left", "0"),
                             str_pad(nviv, 3, "left", "0"))) %>% 
  left_join(muestra %>% 
              select(id_viv_car, id_upm, panelm, vivienda), by = "id_viv_car", suffix = c("", "_c")) %>% 
  select(id_viv_car, id_upm, panelm  = panelm_c, vivienda = vivienda_c,
         hogar, persona = p01, sexo = p02, edad = p03, p06)

# correr antes del select
sum(personas_20$viv_seguimiento == personas_20$vivienda_c)
sum(personas_20$panelm == personas_20$panelm_c)

personas <- import(n_personas) %>% 
  # filtramos galapagos
  filter(prov != "20") %>% 
  mutate(viv_seguimiento =  str_pad(viv_seguimiento, 2, "left", "0"),
         panelm = str_pad(panelm, 3, "left", "0"),
         id_viv_car = paste0(str_pad(prov, 2, "left", "0"),
                             str_pad(cant, 2, "left", "0"),
                             str_pad(parr, 2, "left", "0"),
                             str_pad(zona, 3, "left", "0"),
                             str_pad(sector, 3, "left", "0"),
                             str_pad(manz, 3, "left", "0"),
                             str_pad(edif, 3, "left", "0"),
                             str_pad(nviv, 3, "left", "0"))) %>% 
  left_join(muestra %>% 
              select(id_viv_car, panelm, vivienda), by = "id_viv_car", suffix = c("", "_c")) %>% 
  select(id_viv_car, id_upm = id_conglomerado, panelm, vivienda = viv_seguimiento, #panelm = panelm_c, vivienda = vivienda_c,
         hogar, persona = p01, sexo = p02, edad = p03, p06) %>% 
  # emparejamos galapagos
  rbind(personas_20) %>% 
  mutate(id_upm = str_pad(id_upm, 12, "left", "0"),
         panelm = str_pad(panelm, 3, "left", "0"),
         vivienda = str_pad(vivienda, 2, "left", "0"),
         hogar = str_pad(hogar, 2, "left", "0"),
         persona = str_pad(persona, 2, "left", "0")) %>% 
  # emparejamos el área de la muestra
  left_join(muestra %>%
              group_by(id_upm, area, estrato) %>%
              summarise() %>% 
              ungroup(), by = "id_upm") %>% 
  # Corrijamos el emparejamiento con caratula usando el id_viv_car
  # left_join(caratula, by = c("id_upm", "panelm", "vivienda", "hogar")) %>% 
  left_join(caratula %>% 
              select(-id_upm, -panelm, -vivienda), 
            by = c("id_viv_car", "hogar")) %>% 
  # calculo de los grupos de edad
  mutate(n0a14 = ifelse(edad %in% c(0:14), 1, 0),
         n15a99 = ifelse(edad %in% c(15:99), 1, 0), # codigos para registrar la edad no sabe/no responde y mayores a 98/99 años
         id_per = paste0(id_upm, vivienda, hogar, persona)) %>% 
  select(zonal, pro, id_upm, area, estrato, panelm, vivienda, hogar, persona, id_per, mes, cod_cart,
         sexo, edad, n0a14, n15a99, p06, id_viv_car) # resultado_entrevista, res_ent_otros, numpers, fv

colSums(is.na(personas))
n_distinct(personas$id_per) == dim(personas)[1]

if(n_distinct(personas$id_per) != dim(personas)[1]){
  
  index <- unique(personas$id_per[duplicated(personas$id_per)])
  
  repetidos <- personas |> 
    filter(id_per %in% index)
  
  n_distinct(repetidos$id_viv_car)

}

apoyo <- personas %>%
  group_by(id_upm, mes, vivienda, hogar) %>% 
  summarise(n0a14 = sum(n0a14, na.rm = T),
            n15a99 = sum(n15a99, na.rm = T),
            nper = n()) %>% 
  select(id_upm, mes, vivienda, hogar, n0a14, n15a99, nper) %>% 
  ungroup() %>% 
  filter(id_upm %in% muestra$id_upm)

cob_viv <- caratula %>% 
  full_join(apoyo, by = c("id_upm", "mes", "vivienda", "hogar")) %>%
  mutate(provin = substr(id_upm, 1, 2),
         zonal = case_when(provin %in% c("08", "04", "10", "21", "17") ~ "CZ1",
                           provin %in% c("13", "23", "09", "12", "24", "20") ~ 'CZ2',
                           provin %in% c("02", "15", "22", "16", "05", "18", "06") ~ "CZ3",
                           provin %in% c("01", "03", "14", "07", "11", "19") ~ "CZ4",
                           T ~ "LOL"),
         resultado_entrevista = as.numeric(resultado_entrevista),
         fonocelular = as.numeric(fonocelular),
         res_ent_otros = ifelse(is.na(res_ent_otros), 0, res_ent_otros)) %>%
  group_by(zonal, provin, id_upm, cod_cart, panelm, vivienda, id_viv_car) %>% 
  summarise(resultado_entrevista = min(resultado_entrevista, na.rm = T),
            res_ent_otros = min(res_ent_otros, na.rm = T),
            n0a14 = sum(n0a14, na.rm = T),
            n15a99 = sum(n15a99, na.rm = T),
            numpers = sum(numpers, na.rm = T),
            nper = sum(nper, na.rm = T),
            nhog = n(),
            jefehogar_new = first(nom_jefehogar),
            telefono_11 = first(fonoconvencional),
            telefono_21 = first(fonocelular),
            piso_new = first(piso),
            numnum_new = first(nummunicipio),
            calle_new = first(calle)) %>% 
  ungroup() %>% 
  mutate(# resutaldo de entrevista
         tcomp = ifelse(resultado_entrevista==1, 1, 0),
         trecha = ifelse(resultado_entrevista==2, 1, 0),
         tnadie = ifelse(resultado_entrevista==3, 1, 0),
         ttemp = ifelse(resultado_entrevista==4, 1, 0),
         tdeso = ifelse(resultado_entrevista==5, 1, 0),
         tcons = ifelse(resultado_entrevista==6, 1, 0),
         tdest = ifelse(resultado_entrevista==7, 1, 0),
         tnego = ifelse(resultado_entrevista==8, 1, 0),
         totras = ifelse(resultado_entrevista==9, 1, 0),
         # categorias otra razon
         or_viv_abs = ifelse(res_ent_otros==1, 1, 0),
         or_no_viv = ifelse(res_ent_otros==2, 1, 0),
         or_no_edif = ifelse(res_ent_otros==3, 1, 0),
         or_lotev = ifelse(res_ent_otros==4, 1, 0),
         or_bodega = ifelse(res_ent_otros==5, 1, 0),
         or_ronda = ifelse(res_ent_otros==6, 1, 0),
         or_idoneo = ifelse(res_ent_otros==7, 1, 0),
         or_fondo = ifelse(res_ent_otros==8, 1, 0),
         or_peligro = ifelse(res_ent_otros==9, 1, 0),
         or_enemdu = ifelse(res_ent_otros==10, 1, 0),
         #
         totper = ifelse(is.na(numpers), 0, numpers), 
         nper = ifelse(is.na(nper), 0, nper),
         n0a14 = ifelse(is.na(n0a14), 0, n0a14),
         n15a99 = ifelse(is.na(n15a99), 0, n15a99),
         control = ifelse(nper == totper, 1, 0)) %>% 
  left_join(provincia, by = c("provin")) %>% 
  mutate(Provincia = str_to_title(nprovin),
         re = ifelse(tcomp==1, "TRE",
                     ifelse(trecha==1 | res_ent_otros %in% c(6,10), "TNR",
                            ifelse(tnadie==1 | res_ent_otros %in% c(7, 8, 9), "TED", "TNE"))),
         `Tasas de conformidad` = factor(re, levels = c("TNR", "TED", "TNE", "TRE"),
                                         labels = c("TNR", "TED", "TNE", "TRE"))) %>% 
  select(-nprovin)

# fecha de la base de datos
fecha_base <- stringr::str_sub(fecha, -8, -1)

# guardado de las bases de datos
dir.create(paste0("intermedios/05_cobertura/", mes[m]), showWarnings = F)

save(muestra, caratula, personas, cob_viv, fecha_base,
     file = paste0("intermedios/05_cobertura/", mes[m], "/campo.RData"))

