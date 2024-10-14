
rm(list = ls())

{
  library(rio)
  library(tidyverse)
  library(janitor)
  library(readxl)
}

# -----------------------------------------------------------------------------
# Exportando
# -----------------------------------------------------------------------------

base <- readRDS("intermedios/03_enlistamiento/01_concistencia/base.rds")


# -----------------------------------------------------------------------------
# Condición de ocupación
# -----------------------------------------------------------------------------

base %>% 
  group_by(condicón = c_ocup,zonal) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = zonal, values_from = n) %>% 
  adorn_totals(c("col")) %>%
  arrange(condicón) %>% 
  View()

validUTF8(base$c_ocup) %>% sum()

# -----------------------------------------------------------------------------
# Base viviendas ocupadas
# -----------------------------------------------------------------------------

ocupada <- base %>% 
  filter(c_ocup == "particular - ocupada") %>% 
  mutate(
    man_nloc = ifelse(man != "000", man, n_loc),
    id_viv = paste0(pro, can, par, zon, sec, man_nloc, n_umce, n_viv))


# -----------------------------------------------------------------------------
# Novedades detectadas
# -----------------------------------------------------------------------------

  incosistencia <- ocupada %>%  
  group_by(id_viv, id_upm) %>% 
  mutate(no_id_unico = n()) %>% 
  ungroup() %>% 
  mutate(
         c_na_pro = ifelse(is.na(pro), 1, 0),
         c_in_pro = ifelse(!(as.numeric(pro) %in% 1:24), 1, 0),
         c_na_can = ifelse(is.na(can), 1, 0),
         c_in_can = ifelse(!(as.numeric(can) %in% 1:99), 1, 0),
         c_na_par = ifelse(is.na(par), 1, 0),
         c_in_par = ifelse(!(as.numeric(par) %in% 50:99), 1, 0),
         c_na_zon = ifelse(is.na(zon), 1, 0),
         c_in_zon = ifelse(!(as.numeric(zon) %in% 1:999), 1, 0),
         c_na_sec = ifelse(is.na(sec), 1, 0),
         c_in_sec = ifelse(!(as.numeric(sec) %in% 1:999), 1, 0),
         c_na_manloc = ifelse(is.na(man_nloc), 1, 0),
         c_in_manloc = ifelse(!(as.numeric(man_nloc) %in% 1:999), 1, 0),
         c_na_n_edif = ifelse(is.na(n_umce), 1, 0),
         c_in_n_edif = ifelse(!(as.numeric(n_umce) %in% 1:999), 1, 0),
         c_na_n_viv = ifelse(is.na(n_viv), 1, 0),
         c_in_n_viv = ifelse((!as.numeric(n_viv) %in% 1:999), 1, 0),
         c_na_piso_n = ifelse(is.na(piso_n), 1, 0),
         c_in_piso_n = 0,
         no_id_unico = ifelse(no_id_unico > 1, 1, 0),
         c_12_hbt = ifelse(tot_hbt > 12, 1, 0),
         c_no_upm = ifelse(is.na(id_upm), 1, 0),
         c_n_char_upm = ifelse(nchar(id_upm)==12,1,0),
         c_total = c_na_pro + c_in_pro + c_na_can + c_in_can + c_na_par + 
           c_in_par + c_na_zon +  c_in_zon + c_na_sec + c_in_sec + c_na_manloc +
           c_in_manloc + c_na_n_edif + c_in_n_edif + c_na_n_viv + c_in_n_viv + 
           c_na_piso_n + c_in_piso_n + no_id_unico + c_12_hbt + c_no_upm + 
           c_n_char_upm) %>% 
  filter(c_total > 1) 

dim(incosistencia)
names(apply(incosistencia[,60:65], 2, sum) )
s <- apply(incosistencia[,60:65], 2, sum)
s[1]
class(s)
length(s)
names(s)
(incosistencia[s > 0])
# 
# %>% 
#   select(-d_n_edif, -d_n_viv, -man_nloc, -id_viv, -n0a5, -control_nombre, -tipo, -id_upm, -sec_2021) %>% 
#   mutate(control_total = no_id_unico + c_na_pro + c_in_pro + c_na_can + c_in_can + c_na_par + 
#            c_in_par + c_na_zon + c_in_zon + c_na_sec + c_in_sec + c_na_manloc + c_in_manloc + 
#            c_na_n_edif + c_in_n_edif + c_na_n_viv + c_in_n_viv + c_na_piso_n + c_in_piso_n + 
#            c_hbt + c_n0a5 + c_12_hbt + c_no_upm) %>% 
#   filter(control_total > 0) %>% 
#   select(-control_total)