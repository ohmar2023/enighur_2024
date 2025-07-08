
rm(list = ls())

source("rutinas/99_librerias/librerias.R")

# -----------------------------------------------------------------------------
# Lectura base de cobertura
# -----------------------------------------------------------------------------

periodo <- 5
periodo <- str_pad(periodo,2,"left","0")

ruta <- paste0("intermedios/04_cobertura/periodo_",periodo,
               "/cobertura_base_periodo_", periodo, ".rds")
cobertura_base <- readRDS(ruta)
cobertura_base <- cobertura_base %>% filter(periodo == as.numeric(periodo))

# -----------------------------------------------------------------------------
# Condición de ocupación
# -----------------------------------------------------------------------------

c_ocup <- cobertura_base %>% 
  mutate(n_rvo = case_when(rvo == 1 ~ "Completa",
                           rvo == 2 ~ "Rechazo",
                           rvo == 3 ~ "Nadie en casa",
                           rvo == 4 ~ "Vivienda temporal",
                           rvo == 5 ~ "Vivienda desocupada",
                           rvo == 6 ~ "Vivienda en construcción",
                           rvo == 7 ~ "Vivienda inhabitada o destruida",
                           rvo == 8 ~ "Vivienda convertida en negocio",
                           rvo == 9 ~ "Otra razón",
                           rvo == 10 ~ "Rechazo a mitad de la encuesta")) %>% 
  group_by(zonal,n_rvo) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = zonal, values_from = n) %>% 
  arrange(n_rvo) %>% 
  rename("Condición Ocupación" = n_rvo)
  
c_ocup[is.na(c_ocup)] <- 0

# -----------------------------------------------------------------------------
# Tasas de conformidad a nivel provincial
# -----------------------------------------------------------------------------

cobertura_base <- cobertura_base %>% 
  mutate(Elegibilidad = case_when(rvo == 1 ~ "re",
                                  rvo == 2 ~ "nr",
                                  rvo %in% c(4,5,6,7,8,9) ~ "ne",
                                  rvo == 3 ~ "ed",
                                  TRUE ~ "error"))


# -----------------------------------------------------------------------------
# Gráfico tasas por provincia
# -----------------------------------------------------------------------------

graf_tasas_prov <- cobertura_base %>% 
  filter(Elegibilidad != "error") %>% 
  arrange(desc(Elegibilidad)) %>% 
  ggplot(aes(y = pro, fill = Elegibilidad)) +
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
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# -----------------------------------------------------------------------------
# Gráfico upm por zonales
# -----------------------------------------------------------------------------

graf_tasas_zon <- cobertura_base %>% 
  filter(Elegibilidad != "error") %>% 
  arrange(desc(Elegibilidad)) %>% 
  ggplot(aes(y = zonal, fill = Elegibilidad)) +
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
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# -----------------------------------------------------------------------------
# Tasas de conformidad a nivel UPM
# -----------------------------------------------------------------------------

conformidad_upm <- cobertura_base %>%
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
                               TRUE ~ "error"))

graf_tasas_upm <- ggplot(conformidad_upm %>%  
                           group_by(TRE = inter_tre) %>% 
                           summarize(n = n()) %>% 
                           arrange(TRE), 
                         aes(x = TRE, y = n, fill = TRE)) +
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("#FAD211", "#8BB657", "#D2241F" )) +
  scale_color_manual(values = c("#FAD211", "#D2241F", "#8BB657" ))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())


# -----------------------------------------------------------------------------
# Gráfico tasas por periodo COEBRTURA TOTAL
# -----------------------------------------------------------------------------

ruta <- paste0("intermedios/04_cobertura/")
cobertura_base_total <- import(paste0(ruta,"cobertura_base_total.rds"))

graf_tasas_per <- cobertura_base_total %>% 
  filter(Elegibilidad != "error") %>% 
  arrange(desc(Elegibilidad)) %>% 
  ggplot(aes(x = periodo, fill = Elegibilidad)) +
  geom_bar(position = "fill", alpha = 1.25, width = 0.9) + 
  geom_hline(linetype = "dotted", yintercept = 0.8, color = "black", size = 0.8) + 
  scale_y_continuous(breaks = c(0, 0.1, 0.2 ,0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
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
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())





