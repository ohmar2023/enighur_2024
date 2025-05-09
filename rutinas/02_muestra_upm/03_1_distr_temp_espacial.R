# -----------------------------------------------------------------------------
# Se realiza la distribución en espacio y tiempo acorde al vector:
# c(1, 8, 4, 11, 2, 9, 5, 12, 3, 10, 6, 13, 7)
# Para el caso de galapagos se considera las peticiones realziadas por DIES 
# para que las visitas sean realizadas en un determinado orden en cada isla.
# -----------------------------------------------------------------------------

rm(list = ls())

{
library(tidyverse)
library(janitor)
library(rio)
}

# -----------------------------------------------------------------------------
# Lectura de la muestra
# -----------------------------------------------------------------------------

muestra <- readRDS("insumos/02_muestra_upm/muestra.rds")

# -----------------------------------------------------------------------------
# Creamos la variable zonal
# -----------------------------------------------------------------------------

distribucion <- muestra %>% 
  mutate(zonal = case_when(pro %in% c("04","08","10","17","21","25","30") ~ "norte",
                           pro %in% c("02","05","06","15","16","18","22","29") ~ "centro",
                           pro %in% c("09","12","13","20","23","24","26","32","33") ~ "litoral",
                           pro %in% c("01","03","11","07","14","19","27","28","31") ~ "sur")) %>% 
  arrange(zonal,pro,estrato) %>% 
  cbind(periodo = c(1, 8, 4, 11, 2, 9, 5, 12, 3, 10, 6, 13, 7)) %>%  
  mutate(periodo = case_when(id_upm=="130850021601" ~ 9,
                             id_upm=="130851001001" ~ 8,
                             id_upm=="110150006301" ~ 7,
                             id_upm=="110150040901" ~ 4,
                             T ~ periodo )) %>% 
  arrange(zonal,periodo,pro,estrato) %>% 
  cbind(semana = c(1:4)) %>% 
  mutate(semana = (periodo*4)-(4-semana))

# --- SE GENERA UNA PARTICULAR DISTRIBUCIÓN PARA GALAPAGOS

dis_galapagos <- distribucion %>% 
  mutate(islas = substr(id_upm, 1,4),
         pro = substr(id_upm, 1,2),
         islas_2 = substr(id_upm, 1,6)) %>% 
  filter(pro == "20") %>% arrange(islas,1)

# san_crsitobal ---------------------------------------------------------------
san_crsitobal = dis_galapagos %>% 
  filter(islas == "2001" & islas_2 != "200152")

# floreana --------------------------------------------------------------------
floreana <- dis_galapagos %>% 
  filter(islas_2 == "200152")

# santa_cruz ------------------------------------------------------------------
santa_cruz = dis_galapagos %>% 
  filter(islas == "2003")

# isabela ---------------------------------------------------------------------
isabela = dis_galapagos %>% 
  filter(islas == "2002")


nueva_galapagos_2 <- rbind(san_crsitobal[1:16,],
                           isabela[1:6,],
                           santa_cruz[1:9,],
                           isabela[7:11,],
                           floreana,
                           san_crsitobal[17:31,],
                           santa_cruz[10:61,]
) %>% mutate(semana = c(1:52,1:52)) %>% 
  mutate(periodo = ceiling(semana/4))


distribucion <- distribucion %>% filter(pro != "20") %>% 
  rbind(nueva_galapagos_2 %>% select(-islas, -islas_2))

# -----------------------------------------------------------------------------  
# CONTROL CONGLOMERADO (10 DIG) EN EL MISMO PERIODO ---------------------------
# -----------------------------------------------------------------------------

distribucion %>% 
  mutate(id_cong = substr(id_upm,1,10)) %>% 
  group_by(id_cong) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% View()

# CONTROL DE VALANCEO ENTRE SEMESTRES -------------------------------------

distribucion %>% 
  mutate(grupo = case_when(periodo %in% c(1:6) ~ 1,
                           periodo %in% c(8:13) ~ 2,
                           T ~ 100)) %>% 
  group_by(estrato,grupo) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = grupo,values_from = n) %>% 
  mutate(dif = `1` - `2`) %>% 
  View()

# CONTROL ZONAL Y PERIODO -------------------------------------------------

distribucion %>% 
  group_by(zonal,periodo) %>%
  summarise(n = n()) %>% 
  pivot_wider(names_from = periodo,values_from = n) %>% 
  adorn_totals(c("col","row")) %>% 
  View()
  
# CONTROL POR SEMANA Y ZONAL ----------------------------------------------

distribucion %>% 
  group_by(zonal,semana) %>%
  summarise(n = n()) %>% 
  pivot_wider(names_from = semana,values_from = n) %>% 
  adorn_totals(c("col","row")) %>% 
  View()
  
# CONTROL POR SEMANA Y ZONAL ----------------------------------------------

distribucion %>% 
  group_by(pro,periodo) %>%
  summarise(n = n()) %>% 
  pivot_wider(names_from = periodo,values_from = n) %>% 
  adorn_totals(c("col","row")) %>% 
  View()

# EXPORTAR DISTRIBUCION Y MUESTRA -----------------------------------------
ruta_distr <- "productos/02_muestra_upm/"
export(distribucion, paste0(ruta_distr,"muestra_upm_periodo_semana.rds"))

ruta_distr <- "productos/02_muestra_upm/"
export(distribucion %>% select(zonal,pro,area,id_upm,periodo,semana),
       paste0(ruta_distr,"muestra_upm_periodo_semana.xlsx"))

  
  























