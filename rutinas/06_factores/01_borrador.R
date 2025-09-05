
rm(list = ls())

source("rutinas/99_librerias/librerias.R")

# ------------------------------------------------------------------------------
# Base de cobertura total (cobertura acumulada al periodo n)
#-------------------------------------------------------------------------------
  
cobertura_base_total <- import("intermedios/04_cobertura/cobertura_base_total.rds")

# ------------------------------------------------------------------------------
# Marco UPM
# ------------------------------------------------------------------------------

marco_upm <- readRDS("insumos/02_muestra_upm/marco/marco_upm.rds") %>% 
  group_by(estrato) %>% 
  mutate(Nh = sum(Mi)) %>% #cantidad de viviendas por estrato
  ungroup() %>% 
  rename(Ni = Mi) #secambia el nombre, es la cantidad de vivindas por UPM. 

# ------------------------------------------------------------------------------
# Cobertura total
# ------------------------------------------------------------------------------

#cobertura_base_total <- import("intermedios/04_cobertura/cobertura_base_total.rds")  

cobertura_base_total_1 <- cobertura_base_total %>% 
  group_by(id_upm) %>% 
  mutate(totper = sum(numpers, na.rm = TRUE), 
         tothog = n()) %>% 
  ungroup() %>% 
  group_by(id_upm, id_upm_no_orden) %>% 
  mutate(rvo = min(rvo), 
         n_fila = row_number()) %>% 
  filter(n_fila == 1) %>% 
  ungroup() %>% 
  group_by(id_upm, Elegibilidad, totper, tothog) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = Elegibilidad, values_from = n) %>%
  mutate_all(~replace(.,is.na(.),0)) %>% 
  mutate(nr = nr + error) %>% 
  select(-error)



cobertura_base_total_1 %>% 
  mutate(control_12 = ed   +  ne    + nr   +  re  ) %>% 
  #filter(control_12 != 12) %>% 
  View()


# ------------------------------------------------------------------------------
# Leeomos la información de la actualización cartográfica 
#-------------------------------------------------------------------------------

periodo_ref <- 8
source("rutinas/06_factores/03_borrador.R")

# ------------------------------------------------------------------------------
# Probabilidad de primera etapa.
# ------------------------------------------------------------------------------

ruta <- "insumos/05_factores/01_muestra_pii/"

muestra_primera_etapa <- readRDS(paste0(ruta, "muestra.rds")) %>% 
  mutate(levantada = ifelse(id_upm %in% cobertura_base_total$id_upm, 1, 0)) %>% 
  left_join(select(marco_upm, id_upm, Nh, Ni), by = "id_upm") %>% # atoda la muestra le pego información del marco
  left_join(select(base_act_acum_1, id_upm, area,estrato, Ni_enlist, Nh_enlis), # atoda la muestra le pego información de la actualización
            by = c("id_upm", "area", "estrato" )) 

#dim 1848 = 66*4*12
muestra_primera_etapa <- muestra_primera_etapa %>% 
  group_by(estrato) %>% 
  mutate(mh = n(), #cuantos deberia levantar por estrato hasta el final de la encuesta (tamaño por estrato)
         mh_p = sum(levantada), # cuantos se han levantado hasta el momento (tamaño levanatdo)
         pii_aux =  enighur_pii, 
         ki = 12) %>% #Ajuste de cobertura primera etapa.
  ungroup() %>% 
  left_join(cobertura_base_total_1, by = "id_upm") %>% #le pego las tasas 
  select(id_upm, pro, area, estrato, 
         pik = pii_aux, mh, mh_p, Nh, Ni, 
         ki, ne, nr, re, ed, totper, tothog, Ni_enlist, Nh_enlis) %>% 
  filter(!is.na(ne))


colSums(is.na(muestra_primera_etapa))
n_distinct(muestra_primera_etapa)
muestra_primera_etapa %>% filter(is.na(Ni_enlist )) %>% View()
 





