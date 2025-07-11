rm(list = ls())

source("rutinas/99_librerias/librerias.R")

# -----------------------------------------------------------------------------
# Por provincia
# -----------------------------------------------------------------------------

ruta <- paste0("intermedios/04_cobertura/")
cobertura_base_total <- import(paste0(ruta,"cobertura_base_total.rds"))

# -----------------------------------------------------------------------------
# Por provincia
# -----------------------------------------------------------------------------

  cobertura_base_total %>% 
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
  

elegibilidad_por_prov <- cobertura_base_total_1 %>% 
  filter(Elegibilidad != "error") %>% 
  group_by(pro, Elegibilidad) %>% 
  summarise(n = n()) %>%
  group_by(pro) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(porcentaje = round(100 * n / total, 2)) %>% 
  pivot_wider(id_cols = c(-n, -total),
              names_from = Elegibilidad, 
              values_from = porcentaje) 

rio::export(elegibilidad_por_prov, "documentos/comisión manta/ok_elegibilidad_por_prov.xlsx")

# -----------------------------------------------------------------------------
# Por Ciudad Auto
# -----------------------------------------------------------------------------

v_ciu_auto <- c("170150", #Quito
                "090150", #Guayaquil
                "010150",#Cuenca
                "070150", #Machala
                "180150", #Ambato
                "080150", #Esmeraldas
                "230150", #Santo Domingo
                "130850", #Manta
                "110150") #Loja

cobertura_base_total_ciu_auto <- cobertura_base_total %>% 
  mutate(ciu_auto = paste0(pro, can, par)) %>% 
  filter(ciu_auto %in% v_ciu_auto) %>% 
  mutate(n_ciu_auto = case_when( ciu_auto == "010150" ~ "CUENCA",
                                 ciu_auto == "110150" ~ "LOJA_c", #11
                                 ciu_auto == "170150" ~ "QUITO",
                                 ciu_auto == "180150" ~ "AMBATO",
                                 ciu_auto == "070150" ~ "MACHALA", #07
                                 ciu_auto == "080150"   ~ "ESMERALDAS_c", #08
                                 ciu_auto == "090150" ~ "GUAYAQUIL",
                                 ciu_auto == "130850"  ~ "MANTA", #13
                                 ciu_auto == "230150" ~ "SANTO DOMINGO", #23
                                 TRUE ~ "Error"))

a <- cobertura_base_total_ciu_auto %>% 
  filter(Elegibilidad != "error") %>% 
  arrange(desc(Elegibilidad)) %>% 
  ggplot(aes(y = n_ciu_auto, fill = Elegibilidad)) +
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



elegibilidad_por_ciu_auto <- cobertura_base_total_ciu_auto %>% 
  filter(Elegibilidad != "error") %>% 
  group_by(n_ciu_auto, Elegibilidad) %>% 
  summarise(n = n()) %>%
  group_by(n_ciu_auto) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(porcentaje = round(100 * n / total, 2)) %>% 
  pivot_wider(id_cols = c(-n, -total),
              names_from = Elegibilidad, 
              values_from = porcentaje) 


rio::export(elegibilidad_por_ciu_auto, 
            "documentos/comisión manta/ok_elegibilidad_por_ciu_auto.xlsx")


# -----------------------------------------------------------------------------
# Por zonales
# -----------------------------------------------------------------------------
  
  cobertura_base_total %>% 
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
  group_by(TRE = inter_tre) %>% 
  summarize(n = n()) 

ggplot(conformidad_upm %>% arrange(TRE), 
       aes(y = TRE, x = n, fill = TRE)) +
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("#FAD211", "#8BB657", "#D2241F" )) +
  scale_color_manual(values = c("#FAD211", "#D2241F", "#8BB657" ))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

