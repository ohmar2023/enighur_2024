

conformidad_upm <- cobertura_base_total %>%
  group_by(id_upm, Elegibilidad) %>% 
  summarise(n = n()) %>%
  pivot_wider(names_from = Elegibilidad, values_from = n) %>%
  mutate_all(~replace(.,is.na(.),0)) %>%
  mutate( tre = re / (re + nr + ne + ed),
          tnr = nr / (re + nr + ne + ed),
          tne = ne / (re + nr + ne + ed),
          ted = ed / (re + nr + ne + ed)) %>% 
  mutate(inter_tre = case_when(tre >= 0.75 ~ "Mayor a 75%",
                               tre >= 0.550 & tre < 0.75 ~ "Entre 50% - 75%",
                               tre >= 0 & tre < 50 ~ "Menor a 50%", 
                               TRUE ~ "error")) %>% 
  mutate(id_conglomerado = substr(id_upm, 1,10), 
         n_rvo_aux = ifelse(tre >= 0.75, "Efectiva", "No efectiva"))


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
  left_join(select(conformidad_upm, id_conglomerado, n_rvo_aux, inter_tre), 
            by = "id_conglomerado")

  upm_geo %>% group_by(zonal, inter_tre) %>% summarise(n = n()) %>% 
    st_drop_geometry() %>%
    filter(!is.na(inter_tre)) %>% 
    adorn_totals() %>% 
    View()
  
  

# ------------------------------------------------------------------------------
# Evaluación efectividad de la muestra - zonal hasta el periodo acumulado n.
#-------------------------------------------------------------------------------

z_1 = "ADM. C. CAMPO"

provincias_zonal <- provincias %>% filter(zonal == z_1) 
#cantones_zonal <- cantones %>% filter(zonal == z_1)
upm_geo_zonal <- upm_geo %>% filter(zonal == z_1) %>% 
  filter(id_conglomerado %in% cobertura_base_total$id_conglomerado)

mapa_upm_cobertura_central <- ggplot(data = provincias_zonal) +
  geom_sf(data = provincias_zonal, fill = "#FFF8DC") +
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
#cantones_zonal <- cantones %>% filter(zonal == z_1)
upm_geo_zonal <- upm_geo %>% filter(zonal == z_1) %>% 
  filter(id_conglomerado %in% cobertura_base_total$id_conglomerado)

mapa_upm_cobertura_centro <- ggplot(data = provincias_zonal) +
  geom_sf(data = provincias_zonal, fill = "#FFF8DC") +
  geom_sf(data = upm_geo_zonal, 
          aes(color = as.factor(n_rvo_aux))) + 
  guides(color = guide_legend(title = ""))+
  labs(title = "Evaluación efectividad de la muestra - CENTRO")+
  theme(plot.title = element_text(size = 10))+
  #theme_stata()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

mapa_upm_cobertura_centro


# -----------------------------------------------------------------------------

z_1 = "LITORAL"

provincias_zonal <- provincias %>% filter(zonal == z_1) 
#cantones_zonal <- cantones %>% filter(zonal == z_1)
upm_geo_zonal <- upm_geo %>% filter(zonal == z_1) %>% 
  filter(id_conglomerado %in% cobertura_base_total$id_conglomerado)

mapa_upm_cobertura_litoral <- ggplot(data = provincias_zonal) +
  geom_sf(data = provincias_zonal, fill = "#FFF8DC") +
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
#cantones_zonal <- cantones %>% filter(zonal == z_1)
upm_geo_zonal <- upm_geo %>% filter(zonal == z_1) %>% 
  filter(id_conglomerado %in% cobertura_base_total$id_conglomerado)

mapa_upm_cobertura_sur <- ggplot(data = provincias_zonal) +
  geom_sf(data = provincias_zonal, fill = "#FFF8DC") +
  geom_sf(data = upm_geo_zonal, 
          aes(color = as.factor(n_rvo_aux))) + 
  guides(color = guide_legend(title = ""))+
  labs(title = "Evaluación efectividad de la muestra - SUR")+
  theme(plot.title = element_text(size = 10))+
  #theme_stata()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())



