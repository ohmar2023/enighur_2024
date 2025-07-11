
#------------------------------------------------------------------------------
# Función para graficar la ELEGIBILIDAD
#-----------------------------------------------------------------------------

forma_de_agrupar <- "pro"
variable <- "Elegibilidad"
  
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

# Gráfico por provincia -------------------------------------------------------

graf_elegibilidad_prov_del_per <- graf_elegibilidad(cobertura_base,
                                                    forma_de_agrupar = "pro", 
                                                    variable = "Elegibilidad")

# Gráfico upm por zonales -----------------------------------------------------


graf_elegibilidad_zon_del_per <- graf_elegibilidad(cobertura_base,
                                                   forma_de_agrupar = "zonal", 
                                                   variable = "Elegibilidad")
  

# *****************************************************************************
# Elegibilidad - Por periodo
# *****************************************************************************

# Gráfico tasas por periodo COEBRTURA TOTAL -----------------------------------


ruta <- paste0("intermedios/04_cobertura/")
cobertura_base_total <- import(paste0(ruta,"cobertura_base_total.rds"))

graf_elegibilidad_por_per <- graf_elegibilidad(cobertura_base_total,
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








