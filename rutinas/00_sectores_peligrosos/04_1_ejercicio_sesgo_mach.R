
rm(list = ls())

{
library(srvyr)
library(sampling)
library(survey)
library(sampling)
library(tidyverse)
library(TeachingSampling)
}
  
# Tamaño de UPM en cada estrato ENIGHUR ---------------------------------------- 
distr_estratos_enighur <- readRDS("insumos/02_muestra_upm/pobl_machala/distr_estratos_enighur.rds")

# MARCO UPM --------------------------------------------------------------------
marco_upm <- readRDS("insumos/02_muestra_upm/marco/marco_upm.rds")

# Base que especifica la particion de las SUPER MANZANAS -----------------------
part_man <- readRDS("insumos/02_muestra_upm/pobl_machala/particion_manzanas_li_60.rds")

# Base a nivel de personas para los calculos de indices y estimaciones ---------
poblacion2022_1 <- readRDS("insumos/02_muestra_upm/pobl_machala/poblacion2022_machala.rds") %>% 
  mutate(id_edif = substr(id_viv,1,18)) %>%
  rename(id_conglo = id_upm) %>% 
  left_join(part_man,by = "id_edif") %>% 
  mutate(grupo = ifelse(is.na(grupo), "01", grupo ),
         grupo =  str_pad(grupo, 2, "left", pad = "0"),
         id_upm = paste0(id_conglo,grupo))

# ------------------------------------------------------------------------------
# A continuación se crean vectores para identificar las zonas peligrosas
# ------------------------------------------------------------------------------

# identificadores de UPM. Estas las encontramos en id_upm
machala_peligroso <- c("070150001001",
                       "070150001701",
                       "070150002401",
                       "070150005601",
                       "070150016201",
                       "070150016401",
                       "070150022201",
                       "070150032201",
                       "070150044001",
                       "070150044201",
                       "070150045501",
                       "070150045601",
                       "070150046401",
                       "070150071101",
                       "070150071201",
                       "070150071601",
                       "070150078401",
                       "070150085401",
                       "070150089401")
# Identificadores de ZONAS. Estas zonas las vamos a encontrar en man_sec
machala_zonas <- c("070150002",
                   "070150003",
                   "070150004",
                   "070150005",
                   "070150006",
                   "070150007",
                   "070150008",
                   "070150009",
                   "070150015",
                   "070150016",
                   "070150017",
                   "070150064",
                   "070150066")
# Se filtran las UPM peligrosas en la base de personas
base_1 <- poblacion2022_1 %>% 
  mutate( zonas = substr(man_sec,1,9)) %>% 
  mutate ( peligro = case_when(id_upm %in% machala_peligroso ~ 0,
                               zonas %in% machala_zonas ~ 0, T ~ 1)) %>% 
  group_by(id_upm) %>% 
  mutate(peligro = mean(peligro)) %>% 
  ungroup() %>% 
  filter(peligro == 1)
# Tamaño de la muestra por estratos para la ciudad de MACHALA (40)
tam_estrato <- distr_estratos_enighur %>% 
  filter(grepl(estrato,pattern = "40"))
# Marco de UPM
  marco <- marco_upm 
# Se calculan las probabilidades de selección de UPM-PPT
  marco_upm <- marco %>% 
    filter(id_upm %in% base_1$id_upm) 
   
 #Definimos el marco de viviendas
  marco_viv <- base_1 %>% group_by(id_upm,id_viv) %>% 
    summarise() %>% 
    ungroup() 
  # ---------------------------------------------------------------------------
  # Planteamos una simulación del proceso 200 veces
  # ---------------------------------------------------------------------------
  
 resultante <- NULL
  for(i in c(1:200)){ 
    
    #estan las 124 UPM 
    
   # Selección de la muestra a nivel de UPM
  muestra = marco_upm %>% 
    right_join(tam_estrato) %>% 
    arrange(estrato, desc(Mi)) %>% 
    group_by(estrato) %>% 
    mutate(pik = inclusionprobabilities(Mi, unique(nh)),
           sel = UPrandomsystematic(pik, eps=1e-6)) %>% 
    filter(sel == 1)
    # Selección de vividas dentro de cada UPM
    select_viv <- marco_viv %>% 
    filter(id_upm %in% muestra$id_upm) %>% 
    group_by(id_upm) %>%
    sample_n(12)
  
  muestra_pobl <- muestra %>% 
    mutate(f_exp = Mi/(pik*12)) %>% 
    select(id_upm,estrato,f_exp) 
  
  #personas de las viv seleccionadas
  base_2 <- base_1 %>% filter(id_viv %in% select_viv$id_viv) %>% 
    left_join(select(muestra_pobl,f_exp,id_upm),by="id_upm")

  dis <- base_2 %>% as_survey_design(ids = id_upm,
                                           strata = estrato,
                                           weights = f_exp,
                                           nest = T)
  options(survey.lonely.psu = "adjust")
  
  
  nd_prov_mean <- dis %>%  group_by(pro) %>% 
    summarise(d1 = survey_mean(DO_CIET19,#DE_CIET19, 
                               vartype=c("se","ci","cv","var"),
                               na.rm = T,deff = T),n=n(),N=sum(f_exp)) 

  resultante <- rbind(resultante, nd_prov_mean)
}

  mean(resultante$d1)*100











  
  
  
  
  
  
  
  
  
  