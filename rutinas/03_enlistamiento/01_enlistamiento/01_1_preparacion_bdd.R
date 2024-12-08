# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

rm(list = ls())

{
  library(rio)
  library(tidyverse)
  library(janitor)
  library(readxl)
  library(stringi)
}

# -----------------------------------------------------------------------------
# Lectura base de enlistamiento
# -----------------------------------------------------------------------------

base <- read_excel("insumos/03_enlistamiento/2024_11_28/BaseMuestralCompletaParaDinem25_11_2024.xlsx",
                   guess_max = 10000) %>% 
  clean_names() %>% 
  mutate(c_ocup = tolower(c_ocup),
         primernjh = toupper(stri_trans_general(primernjh,"Latin-ASCII")),
         segundonjh	= toupper(stri_trans_general(segundonjh,"Latin-ASCII")),
         primerajh = toupper(stri_trans_general(primerajh,"Latin-ASCII")),
         segudonjh = toupper(stri_trans_general(segudonjh,"Latin-ASCII")),
         id_upm = upm,
         tot_hbt = as.numeric(tot_hbt))

marco_upm <- readRDS("insumos/02_muestra_upm/marco/marco_upm.rds")
# -----------------------------------------------------------------------------
# Agregando la variable: zonal
# -----------------------------------------------------------------------------

base <- base %>% 
  mutate(zonal = case_when(pro %in% c("04","08","10","17","21","25","30") ~ "ADM. C. CAMPO",
                           pro %in% c("02","05","06","15","16","18","22","29") ~ "CENTRO",
                           pro %in% c("09","12","13","20","23","24","26","32","33") ~ "LITORAL",
                           pro %in% c("01","03","11","07","14","19","27","28","31") ~ "SUR")) 

# -----------------------------------------------------------------------------
# Controlando los caracteres en variables
# -----------------------------------------------------------------------------

base <- base %>% 
  mutate(pro = str_pad(pro, 2, "left", "0"),
         can = str_pad(can, 2, "left", "0"),
         par = str_pad(par, 2, "left", "0"),
         zon = str_pad(zon, 3, "left", "0"),
         sec = str_pad(sec, 3, "left", "0"),
         man = str_pad(man, 3, "left", "0"),
         n_loc = str_pad(n_loc, 3, "left", "0"),
         n_umce = str_pad(n_umce, 3, "left", "0"),
         n_viv = str_pad(n_viv, 4, "left", "0"),
         primernjh = gsub(" ", "", primernjh),
         segundonjh = gsub(" ", "", segundonjh),
         primerajh = gsub(" ", "",primerajh),
         segudonjh =gsub(" ", "",segudonjh),
         tot_hbt = ifelse(is.na(tot_hbt) | tot_hbt == "", 0, tot_hbt)) %>% 
  replace(. == "",NA) %>% 
  mutate(man_sec_21 = ifelse(zon == "999", paste0(pro, can, par, zon, sec), 
                             paste0(pro, can, par, zon, sec, man)))
# -----------------------------------------------------------------------------
# Exportando
# -----------------------------------------------------------------------------

export(base, "intermedios/03_enlistamiento/01_concistencia/base.rds")

