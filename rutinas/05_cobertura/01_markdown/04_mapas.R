
# -----------------------------------------------------------------------------
# Gráfico tasas MAPA COEBRTURA TOTAL
# -----------------------------------------------------------------------------

#source("rutinas/99_librerias/librerias.R")

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
# Base de cobertura total (cobertura acumulada al periodo n)
#-------------------------------------------------------------------------------

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
  left_join(select(tasas_conf_upm %>% ungroup(), id_conglomerado, inter_tre, n_rvo_aux), by = "id_conglomerado")

upm_geo%>% names()

# ------------------------------------------------------------------------------
# Función mapas por zonales
#-------------------------------------------------------------------------------

graf_mapas_upm <- function(z_1){
  
  provincias_zonal <- provincias %>% 
    filter(zonal == z_1)
  
  upm_geo_zonal <- upm_geo %>% filter(zonal == z_1) %>% 
    filter(id_conglomerado %in% cobertura_base_total$id_conglomerado)
  
  mapa_upm_cobertura <- ggplot(data = provincias_zonal) +
    geom_sf(data = provincias_zonal, fill = "#FFF8DC") +
    geom_sf(data = upm_geo_zonal, 
            aes(color = as.factor(n_rvo_aux))) + 
    guides(color = guide_legend(title = ""))+
    labs(title = paste0("Evaluacion efectividad de la muestra - ", z_1))+
    theme(plot.title = element_text(size = 10))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())
}

#-------------------------------------------------------------------------------

z_1 = "ADM. C. CAMPO"
mapa_upm_cobertura_central <- graf_mapas_upm(z_1)
  
# -----------------------------------------------------------------------------

z_1 = "CENTRO"
mapa_upm_cobertura_centro <- graf_mapas_upm(z_1)

# -----------------------------------------------------------------------------

z_1 = "LITORAL"
mapa_upm_cobertura_litoral <- graf_mapas_upm(z_1)

# -----------------------------------------------------------------------------

z_1 = "SUR"
mapa_upm_cobertura_sur <- graf_mapas_upm(z_1)

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
