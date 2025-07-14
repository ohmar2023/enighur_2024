
rm(list = ls())

source("rutinas/99_librerias/librerias.R")

# ------------------------------------------------------------------------------
#
# ------------------------------------------------------------------------------

muestra_usm_acu <- NULL

for (i in c(1:5)){
  periodo <- i
  periodo <- str_pad(periodo, 2, "left", "0")
  if(periodo == "01"){
    ruta <- paste0("productos/02_muestra_usm/","periodo_", periodo,"/muestra_periodo_01_myc.xlsx")
  }else{
  ruta <- paste0("productos/02_muestra_usm/","periodo_", periodo,"/muestra_usm_myc.xlsx")
  }
  aux <- import(ruta)
  muestra_usm_acu <- rbind(muestra_usm_acu, aux)
}

sn_1 <- muestra_usm_acu %>% 
  filter(provin != "20") %>% 
  mutate(sn = ifelse(jefehoga == "Sin Nombre", "Sin_Nombre", "Con_Nombre")) %>% 
  group_by(zonal, sn) %>% 
  summarise(n = n()) %>% 
  group_by(zonal) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(porcentaje = round(n / total, 2)) %>% 
  pivot_wider(id_cols = c(-n, -total),
              names_from = sn, 
              values_from = porcentaje) 

rio::export(sn_1, "documentos/comisión manta/ok_sn_1.xlsx")

# -----------------------------------------------------------------------------
# Tasas de conformidad a nivel UPM
# -----------------------------------------------------------------------------
# 
# conformidad_upm <- cobertura_base_total %>%
#   group_by(id_upm, Elegibilidad) %>% 
#   summarise(n = n()) %>%
#   pivot_wider(names_from = Elegibilidad, values_from = n) %>%
#   mutate_all(~replace(.,is.na(.),0)) %>%
#   mutate( tre = re / (re + nr + ne + ed),
#           tnr = nr / (re + nr + ne + ed),
#           tne = ne / (re + nr + ne + ed),
#           ted = ed / (re + nr + ne + ed)) %>% 
#   mutate(inter_tre = case_when(tre >= 0.75 ~ "Mayor a 75%",
#                                tre >= 0.550 & tre < 0.75 ~ "Entre 50% - 75%",
#                                tre >= 0 & tre < 50 ~ "Menor a 50%", 
#                                TRUE ~ "error"))
# 
# graf_tasas_upm <- ggplot(conformidad_upm %>%  
#                            group_by(TRE = inter_tre) %>% 
#                            summarize(n = n()) %>% 
#                            arrange(TRE), 
#                          aes(x = TRE, y = n, fill = TRE)) +
#   geom_bar(stat = "identity")+
#   scale_fill_manual(values = c("#FAD211", "#8BB657", "#D2241F" )) +
#   scale_color_manual(values = c("#FAD211", "#D2241F", "#8BB657" ))+
#   theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# lol <- cobertura_base_total %>% mutate(aux = paste0(id_upm, vivienda))
# n_distinct(lol$aux)
# dim(cobertura_base_total)
# lol %>% filter(duplicated(aux)) %>% View
# 
# cobertura_base_total %>% group_by(id_upm, vivienda, hogar, n_rvo) %>% 
#   summarise(n()) %>% View()

sn_2 <- muestra_usm_acu %>%
  filter(provin != "20") %>% 
  left_join(select(cobertura_base_total,
                    no_orden = vivienda,
                   id_upm, n_rvo), by = c("id_upm", "no_orden")) %>% 
  mutate(sn = ifelse(jefehoga == "Sin Nombre", "Sin_Nombre", "Con_Nombre")) %>% 
  group_by(zonal, sn, n_rvo) %>% 
  summarise(n = n()) %>% 
  group_by(zonal, sn) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(porcentaje = round(n / total, 4)) %>% 
  pivot_wider(id_cols = c(-n, -total),
              names_from = sn, 
              values_from = porcentaje) 


rio::export(sn_2, "documentos/comisión manta/ok_sn_2.xlsx")
# -----------------------------------------------------------------------------
#
# -----------------------------------------------------------------------------

muestra_usm_acu %>%  
  filter(jefehoga == "Sin Nombre") %>% 
  left_join( cobertura_base_total %>%
               filter(!duplicated(id_upm)) %>% 
    mutate(n_rvo_aux = ifelse(n_rvo == "Completa",
                              "Efectiva",
                              "No efectiva")) %>% select(id_upm,n_rvo_aux),
    by = "id_upm") %>%
  filter(!is.na(n_rvo_aux)) %>%
  group_by(provin, n_rvo_aux) %>%
  summarise(n()) %>% 
  View()
  

#1299 sin nombre hasta el periodo 9



cobertura_base_total %>% names()

table(cobertura_base_total$n_rvo)















