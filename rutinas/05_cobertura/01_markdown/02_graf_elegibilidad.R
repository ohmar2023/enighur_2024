
#------------------------------------------------------------------------------
# Función para graficar la ELEGIBILIDAD
#-----------------------------------------------------------------------------

graf_elegibilidad <- function(base,
                              forma_de_agrupar = "pro",
                              variable = "Elegibilidad"){
  base %>% 
  filter(Elegibilidad != "error") %>% 
  arrange(desc(Elegibilidad)) %>% 
  ggplot(aes(y = .data[[forma_de_agrupar]], 
             fill = .data[[variable]])) +
  geom_bar(position = "fill", alpha = 1.25, width = 0.9) + 
  
  geom_vline(linetype = "dotted", xintercept = 0.8, color = "black", size = 0.8) + 
  geom_vline(linetype = "dotted", xintercept = 0.7, color = "white", size = 0.8)+
  geom_vline(linetype = "dotted", xintercept = 0.6, color = "white", size = 0.8)+
  geom_vline(linetype = "dotted", xintercept = 0.5, color = "white", size = 0.8)+
  geom_vline(linetype = "dotted", xintercept = 0.55, color = "white", size = 0.8)+
  geom_vline(linetype = "dotted", xintercept = 0.65, color = "white", size = 0.8)+
  geom_vline(linetype = "dotted", xintercept = 0.75, color = "white", size = 0.8)+
    
    scale_x_continuous(breaks = c(0, 0.1, 0.2 ,0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
                     labels = c("0" = "0%", "0.1" = "10%", "0.2" = "20%" ,"0.3"="30%", 
                                "0.4" = "40%", "0.5"="50%", "0.6"="60%", "0.7"="70%", 
                                "0.8" = "80%", "0.9"="90%", "1.0"="100%"))  +
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.background = element_rect(fill="white"),
        axis.text = element_text(size=7),
        axis.title = element_text(size=9),
        axis.title.x = element_blank(),
        legend.box = "vertical",
        legend.box.spacing = unit(0.5, "cm"),
        legend.direction = "vertical", 
        legend.position = "right",
        legend.text = element_text(size=7),
        legend.title = element_text(size=8),
        legend.title.align = 0.5,
        panel.border = element_rect(colour = "black", fill = NA))  +
  scale_fill_manual(values = c("#FAD211","orange2", "#D2241F", "#8BB657" )) + 
  scale_color_manual(values = c("#FAD211","orange2", "#D2241F", "#8BB657" ))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())}



# *****************************************************************************
# Elegibilidad - PERIODO
# *****************************************************************************
# 
# 
# # Gráfico por provincia -------------------------------------------------------
# 
# graf_elegibilidad_prov_del_per <- graf_elegibilidad(cobertura_base,
#                                                     forma_de_agrupar = "pro", 
#                                                     variable = "Elegibilidad")
# 
# # Gráfico upm por zonales -----------------------------------------------------
# 
# 
# graf_elegibilidad_zon_del_per <- graf_elegibilidad(cobertura_base,
#                                                    forma_de_agrupar = "zonal", 
#                                                    variable = "Elegibilidad")
  

# *****************************************************************************
# Elegibilidad - Acumulada
# *****************************************************************************

# Gráfico tasas por periodo COEBRTURA TOTAL -----------------------------------


graf_elegibilidad_por_per <- graf_elegibilidad(cobertura_base_total %>% 
                                                 mutate(periodo = str_pad(periodo, 2, "left" , "0")) %>% 
                                                 arrange(desc(periodo)),
                                               forma_de_agrupar = "periodo", 
                                                  variable = "Elegibilidad")


# *****************************************************************************
# Elegibilidad - ACUMULADA
# *****************************************************************************

# Gráfico ACUMULADO  ----------------------------------------------------------

graf_elegibilidad_prov_acum <- graf_elegibilidad(cobertura_base_total,
                                                    forma_de_agrupar = "pro", 
                                                    variable = "Elegibilidad")


graf_elegibilidad_nac_acum <- graf_elegibilidad(cobertura_base_total %>% 
                                                   mutate(nacional = ""),
                                                 forma_de_agrupar = "nacional", 
                                                 variable = "Elegibilidad")
# Gráfico upm por zonales -----------------------------------------------------

graf_elegibilidad_zon_acum <- graf_elegibilidad(cobertura_base_total,
                                                   forma_de_agrupar = "zonal", 
                                                   variable = "Elegibilidad")

# Gráfico upm por cantón autorepresentado  ------------------------------------

# Agregando estrato


marco_upm <- readRDS("insumos/02_muestra_upm/marco/marco_upm_23.rds")

# v_canton_auto <- c("1701", #Quito
#                    "0901", #Guayaquil
#                    "0101",#Cuenca
#                    "0701", #Machala
#                    "1801", #Ambato
#                    "0801", #Esmeraldas
#                    "2301", #Santo Domingo
#                    "1308", #Manta
#                    "1101") #Loja

cobertura_base_total_canton_auto <- cobertura_base_total %>% 
  left_join(select(marco_upm,estrato,id_upm),by = "id_upm" ) %>% 
  mutate(canton_auto = paste0(pro, can)) %>% 
  filter(substr(estrato,1,2) >= 30 & substr(estrato,1,2) != "31") %>% 
  mutate(n_canton_auto = case_when( canton_auto == "0101" ~ "CUENCA",
                                    canton_auto == "1101" ~ "LOJA_c", #11
                                    canton_auto == "1701" ~ "QUITO",
                                    canton_auto == "1801" ~ "AMBATO",
                                    canton_auto == "0701" ~ "MACHALA", #07
                                    canton_auto == "0801"   ~ "ESMERALDAS_c", #08
                                    canton_auto == "0901" ~ "GUAYAQUIL",
                                    canton_auto == "1308"  ~ "MANTA", #13
                                    canton_auto == "2301" ~ "SANTO DOMINGO", #23
                                    TRUE ~ "Error"))


graf_elegibilidad_canton_auto_acum <- graf_elegibilidad(cobertura_base_total_canton_auto,
                                                forma_de_agrupar = "n_canton_auto", 
                                                variable = "Elegibilidad")

# -----------------------------------------------------------------------------
# Cobertura por canton auto y estrato
# -----------------------------------------------------------------------------

cobertura_total_canton_estrato <- cobertura_base_total_canton_auto %>% 
  mutate(estrato_nombre = paste0(n_canton_auto,"-", substr(estrato, 3,3), "-",substr(estrato, 4,4)))


graf_elegibilidad_canton_auto_acum_estrato <- graf_elegibilidad(cobertura_total_canton_estrato,
                  forma_de_agrupar = "estrato_nombre", 
                  variable = "Elegibilidad")

