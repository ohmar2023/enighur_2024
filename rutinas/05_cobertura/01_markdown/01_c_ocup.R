
rm(list = ls())

source("rutinas/99_librerias/librerias.R")
#source("rutinas/05_cobertura/01_markdown/02_graf_elegibilidad.R")
#source("rutinas/05_cobertura/01_markdown/02_graf_elegibilidad.R")
#source("rutinas/05_cobertura/01_markdown/03_graf_tasas_conformidad.R")
#source("rutinas/05_cobertura/01_markdown/04_mapas.R")

# -----------------------------------------------------------------------------
# Lectura base de cobertura
# -----------------------------------------------------------------------------

periodo <- 7
periodo <- str_pad(periodo,2,"left","0")

ruta <- paste0("intermedios/04_cobertura/periodo_",periodo,
               "/cobertura_base_periodo_", periodo, ".rds")
cobertura_base <- readRDS(ruta)
cobertura_base <- cobertura_base %>% filter(periodo == as.numeric(periodo))

# -----------------------------------------------------------------------------
# Condición de ocupación - del periodo
# -----------------------------------------------------------------------------

c_ocup <- cobertura_base %>% 
  group_by(zonal, n_rvo) %>%
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
# Condición de ocupación - acumulada
# -----------------------------------------------------------------------------
ruta <- paste0("intermedios/04_cobertura/")
cobertura_base_total <- import(paste0(ruta,"cobertura_base_total.rds"))

c_ocup_acum <- cobertura_base_total %>% 
  group_by(zonal, n_rvo) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = zonal, values_from = n) %>% 
  arrange(n_rvo) %>% 
  #filter(!is.na(n_rvo)) %>% 
  rename("Condición Ocupación" = n_rvo) 

c_acum_aux <- c_ocup_acum[,c(2:5)]
c_acum_aux[is.na(c_acum_aux)] <- 0

ifelse(is.na(c_ocup_acum[,c(2:5)]), "hola", "xxx") 

c_ocup_acum <- cbind(c_ocup_acum[,1], c_acum_aux)


