
# -----------------------------------------------------------------------------
# Gráfico tasas MAPA COEBRTURA TOTAL
# -----------------------------------------------------------------------------

#rm(list = ls())
source("rutinas/99_librerias/librerias.R")

# ------------------------------------------------------------------------------
# PROVINVIAS
#-------------------------------------------------------------------------------

provincias <- read_sf("insumos/99_gpk_upm_muestra/SHP/nxprovincias.shp") %>% 
  clean_names() %>% 
  mutate(zonal = case_when(dpa_provin  %in% c("04","08","10","17","21","25","30") ~ "ADM. C. CAMPO",
                           dpa_provin  %in% c("02","05","06","15","16","18","22","29") ~ "CENTRO",
                           dpa_provin  %in% c("09","12","13","23","24","26","32","33") ~ "LITORAL",
                           dpa_provin  %in% c("01","03","11","07","14","19","27","28","31") ~ "SUR")) 

# ------------------------------------------------------------------------------
# CANTONES
#-------------------------------------------------------------------------------

cantones <- read_sf("insumos/99_gpk_upm_muestra/SHP/nxcantones.shp") %>%  
  clean_names() %>% 
  mutate(dpa_provin = substr(dpa_canton ,1,2)) %>% 
  mutate(zonal = case_when(dpa_provin  %in% c("04","08","10","17","21","25","30") ~ "ADM. C. CAMPO",
                           dpa_provin  %in% c("02","05","06","15","16","18","22","29") ~ "CENTRO",
                           dpa_provin  %in% c("09","12","13","23","24","26","32","33") ~ "LITORAL",
                           dpa_provin  %in% c("01","03","11","07","14","19","27","28","31") ~ "SUR")) 

# ------------------------------------------------------------------------------
# PARROQUIAS
#-------------------------------------------------------------------------------

# parroquias <- read_sf("insumos/99_gpk_upm_muestra/SHP/nxparroquias.shp")
# names(parroquias)
# 
# table(parroquias$DPA_PROVIN)
# 
# parroquias %>% 
#   filter(!DPA_PROVIN %in% c("20")) %>%   
#   ggplot() +
#   geom_sf()
# 
# #st_drop_geometry: quita la geometria a la base de datos
# as.data.frame(unique(st_drop_geometry(Cantones[,c("DPA_DESPRO","DPA_PROVIN")]))) %>% 
#   arrange(DPA_PROVIN)

# ------------------------------------------------------------------------------
# Base de cobertura total (cobertura acumulada al periodo n)
#-------------------------------------------------------------------------------

cobertura_base_total <- import("intermedios/04_cobertura/cobertura_base_total.rds") %>% 
  mutate(id_conglomerado = substr(id_upm, 1,10), 
         n_rvo_aux = ifelse(n_rvo == "Completa", "Efectiva", "No efectiva"), 
         n_rvo_aux = ifelse(is.na(n_rvo_aux), "No efectiva", n_rvo_aux))

periodo_referencia_mapa <- max(cobertura_base_total$periodo)

# ------------------------------------------------------------------------------
# Polígonos de las UPM: Se saca el centroide para poder ubicarlas en el mapa como puntos.
# n_rvo_aux: No efectiva y Efectiva.
# upm_geo: Tiene n_rvo_aux hasta el periodo acumulado n.
#-------------------------------------------------------------------------------

upm_geo <- read_sf("insumos/99_gpk_upm_muestra/muestra_conglomerado_geo.gpkg") %>% 
  st_centroid() %>% 
  rename(id_conglomerado = id_upm) %>% 
  mutate(dpa_provin = substr(id_conglomerado, 1, 2),
         upm_levantada = ifelse(id_conglomerado %in% cobertura_base_total$id_conglomerado, 
                                "Levantado", "Por levantar")) %>% 
  mutate(zonal = case_when(dpa_provin  %in% c("04","08","10","17","21","25","30") ~ "ADM. C. CAMPO",
                           dpa_provin  %in% c("02","05","06","15","16","18","22","29") ~ "CENTRO",
                           dpa_provin  %in% c("09","12","13","23","24","26","32","33") ~ "LITORAL",
                           dpa_provin  %in% c("01","03","11","07","14","19","27","28","31") ~ "SUR")) %>% 
  left_join(select(cobertura_base_total, id_conglomerado, n_rvo_aux), by = "id_conglomerado")

  # mutate(centroide = st_centroid(geom),
  #        centroide = as.character(centroide),
  #        centroide = gsub("\\,", "", centroide),
  #        centroide = gsub("\\)|", "", centroide),
  #        centroide = gsub("\\(|", "", centroide),
  #        centroide = gsub("\\c", "", centroide), 
  #        aux = str_locate(centroide, pattern = " ")[1,1], 
  #        centroide_x = substr(centroide, 1, aux), 
  #        centroide_y = substr(centroide, aux + 1, nchar(centroide)))


# ------------------------------------------------------------------------------
# Mapa nacional: Usando n_rvo_aux (efectivas vs. no efectivas NACIONAL)
#-------------------------------------------------------------------------------

prov = "20"  

mapa_upm_cobertura_nacional <- ggplot(data = provincias %>% 
                                        filter(dpa_provin != prov)) +
  #geom_sf(fill = "lightgreen") + # Color de relleno de la primera capa
  geom_sf(data = cantones %>% filter(dpa_provin != prov), fill = "#FFF8DC") +
  geom_sf(data = upm_geo %>% filter(dpa_provin != prov & !is.na(n_rvo_aux)), 
          aes(color = as.factor(n_rvo_aux))) + # Superpone la segunda capa
  guides(color = guide_legend(title = ""))+
  labs(title = "Evaluación de la efectividad de la muestra") +
  theme_stata() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

#mapa_upm_cobertura_nacional

# ------------------------------------------------------------------------------
# Evaluación efectividad de la muestra - zonal hasta el periodo acumulado n.
#-------------------------------------------------------------------------------

z_1 = "ADM. C. CAMPO"
  
provincias_zonal <- provincias %>% filter(zonal == z_1) 
cantones_zonal <- cantones %>% filter(zonal == z_1)
upm_geo_zonal <- upm_geo %>% filter(zonal == z_1) %>% 
  filter(id_conglomerado %in% cobertura_base_total$id_conglomerado)
  
mapa_upm_cobertura_central <- ggplot(data = provincias_zonal) +
  geom_sf(data = cantones_zonal, fill = "#FFF8DC") +
  geom_sf(data = upm_geo_zonal, 
          aes(color = as.factor(n_rvo_aux))) + 
  guides(color = guide_legend(title = ""))+
  labs(title = "Evaluacion efectividad de la muestra - ADM. C. CAMPO")+
  theme(plot.title = element_text(size = 10))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

mapa_upm_cobertura_central

# -----------------------------------------------------------------------------

z_1 = "CENTRO"

provincias_zonal <- provincias %>% filter(zonal == z_1) 
cantones_zonal <- cantones %>% filter(zonal == z_1)
upm_geo_zonal <- upm_geo %>% filter(zonal == z_1) %>% 
  filter(id_conglomerado %in% cobertura_base_total$id_conglomerado)

mapa_upm_cobertura_centro <- ggplot(data = provincias_zonal) +
  geom_sf(data = cantones_zonal, fill = "#FFF8DC") +
  geom_sf(data = upm_geo_zonal, 
          aes(color = as.factor(n_rvo_aux))) + 
  guides(color = guide_legend(title = ""))+
  labs(title = "Evaluación efectividad de la muestra - CENTRO")+
  theme(plot.title = element_text(size = 10))+
  #theme_stata()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

# -----------------------------------------------------------------------------

z_1 = "LITORAL"

provincias_zonal <- provincias %>% filter(zonal == z_1) 
cantones_zonal <- cantones %>% filter(zonal == z_1)
upm_geo_zonal <- upm_geo %>% filter(zonal == z_1) %>% 
  filter(id_conglomerado %in% cobertura_base_total$id_conglomerado)

mapa_upm_cobertura_litoral <- ggplot(data = provincias_zonal) +
  geom_sf(data = cantones_zonal, fill = "#FFF8DC") +
  geom_sf(data = upm_geo_zonal, 
          aes(color = as.factor(n_rvo_aux))) + 
  guides(color = guide_legend(title = ""))+
  labs(title = "Evaluación efectividad de la muestra - LITORAL")+
  theme(plot.title = element_text(size = 10))+
  #theme_stata()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

# -----------------------------------------------------------------------------

z_1 = "SUR"

provincias_zonal <- provincias %>% filter(zonal == z_1) 
cantones_zonal <- cantones %>% filter(zonal == z_1)
upm_geo_zonal <- upm_geo %>% filter(zonal == z_1) %>% 
  filter(id_conglomerado %in% cobertura_base_total$id_conglomerado)

mapa_upm_cobertura_sur <- ggplot(data = provincias_zonal) +
  geom_sf(data = cantones_zonal, fill = "#FFF8DC") +
  geom_sf(data = upm_geo_zonal, 
          aes(color = as.factor(n_rvo_aux))) + 
  guides(color = guide_legend(title = ""))+
  labs(title = "Evaluación efectividad de la muestra - SUR")+
  theme(plot.title = element_text(size = 10))+
  #theme_stata()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

# ------------------------------------------------------------------------------
# Mapa nacional levantadas vs. no levantadas
#-------------------------------------------------------------------------------

prov = "20"  

mapa_muestra_nacional <- ggplot() +
  #geom_sf(data = cantones %>% filter(dpa_provin != prov), fill = "#FFF8DC") +
  geom_sf(data = provincias %>% filter(dpa_provin != prov), 
           alpha = 0.5, fill = "white", color = "black", size = 20) +
  geom_sf(data = upm_geo %>% filter(dpa_provin != prov), 
          aes(color = as.factor(upm_levantada))) + # Superpone la segunda capa
  guides(color = guide_legend(title = ""))+
  #labs(title = "Evaluación del levantamiento de la muestra")+
  theme_stata()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

#mapa_muestra_nacional
