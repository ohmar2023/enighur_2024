
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
  mutate(Nh = sum(Mi)) %>% 
  ungroup() %>% 
  rename( Ni = Mi)

# ------------------------------------------------------------------------------
# Cobertura total
# ------------------------------------------------------------------------------

cobertura_base_total <- import("intermedios/04_cobertura/cobertura_base_total.rds") 

cobertura_base_total_1 <- cobertura_base_total %>% 
  group_by(id_upm) %>% 
  mutate(totper = sum(numpers, na.rm = TRUE), 
         tothog = sum(hogar, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(id_upm, Elegibilidad, totper, tothog) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = Elegibilidad, values_from = n) %>%
  mutate_all(~replace(.,is.na(.),0))
# mutate( tre = re / (re + nr + ne + ed),
#         tnr = nr / (re + nr + ne + ed),
#         tne = ne / (re + nr + ne + ed),
#         ted = ed / (re + nr + ne + ed)) %>% 
# ungroup()

# ------------------------------------------------------------------------------
# Probabilidad de primera etapa.
# ------------------------------------------------------------------------------

ruta <- "insumos/05_factores/01_muestra_pii/"

muestra_primera_etapa <- readRDS(paste0(ruta, "muestra.rds")) %>% 
  mutate(levantada = ifelse(id_upm %in% cobertura_base_total$id_upm, 1, 0)) %>% 
  left_join(select(marco_upm, id_upm, Nh, Ni), by = "id_upm")

muestra_primera_etapa <- muestra_primera_etapa %>% 
  group_by(estrato) %>% 
  mutate(mh = n(),
         mh_p = sum(levantada), 
         pii_aux =  enighur_pii, 
         ki = 12) %>% #Ajuste de cobertura primera etapa.
  ungroup() %>% 
  left_join(cobertura_base_total_1, by = "id_upm") %>% 
  select(id_upm, pro, area, estrato, 
         pik = pii_aux, mh, mh_p, Nh, Ni, 
         ki, ne, nr, re, ed, totper, tothog) %>% 
  filter(!is.na(ne))









