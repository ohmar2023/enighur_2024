
rm(list = ls())

library(srvyr)
source("rutinas/99_librerias/librerias.R")

table(marco_viv_superman$grupo, useNA = "ifany")
table(aux$grupo, useNA = "ifany")

aux <- marco_viv_superman %>% 
  mutate(grupo = case_when(is.na(grupo) ~ "1", 
                           grupo == 0 ~ "2", 
                           T ~ grupo)) %>% 
  mutate(id_upm = paste0(substr(id_upm, 1, 11), grupo ))



n_variables <- c("id_upm", "pro", "can", "par", "zon", "sec", "man", "n_umce", 
                 "n_viv", "piso_n", "id_viv", "n_via_p", "n_pm","area", "estrato", 
                 "zonal", "dpa_pro", "dpa_can", "dpa_parr", "jefe_hogar", "tot_hbt", 
                 "pluscodes", "periodo_nuevo", "semana_nueva",  "celular", "convencional",
                 "id_edif", "id_viv", "man_nloc","estrato","c_ocup")

aux <- aux %>% select(all_of(n_variables))

rio::export(aux, "marco_viv_super_periodo_12.rds")

sum(aux$id_upm == aux$id_upm_aux)
class(marco_viv_superman$grupo)

n_distinct(marco_viv_superman$id_upm)

marco_viv_superman %>% select(id_upm, grupo) %>% View()

# ----------------------------------------------------------------------------


marco_viv_super_periodo_12 <- read_excel("productos/02_muestra_usm/periodo_12/99_ruitna_supermanzanas/marco_viv_super_periodo_12.xlsx")

viviendas_sin_pluscode <- read_excel("productos/02_muestra_usm/periodo_12/99_ruitna_supermanzanas/viviendas_sin_pluscode.xlsx")

names(viviendas_sin_pluscode)
names(marco_viv_super_periodo_12)

