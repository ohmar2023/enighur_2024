
rm(list = ls())

{
  library(rio)
  library(tidyverse)
  library(janitor)
  library(readxl)
  library(openxlsx)
}

# -----------------------------------------------------------------------------
# Actualizar mes (variable mese) : falta considerar en la base
# -----------------------------------------------------------------------------

mese <- 1

# -----------------------------------------------------------------------------
# Importando
# -----------------------------------------------------------------------------
# fecha <- substr(last(list.files("insumos/03_enlistamiento/pentaho/csv/",
#                                 recursive = F, pattern = "enlistamiento")), 15, 22)

fecha <- "20242411"
base <- readRDS("intermedios/03_enlistamiento/01_concistencia/base.rds")
marco <- readRDS("insumos/02_muestra_upm/marco/marco_upm.rds")

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

  inconsistencia <- ocupada %>%  
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
         c_n_char_upm = ifelse(nchar(id_upm) != 12,1,0),
         
         c_total = c_na_pro + c_in_pro + c_na_can + c_in_can + c_na_par + 
           c_in_par + c_na_zon +  c_in_zon + c_na_sec + c_in_sec + c_na_manloc +
           c_in_manloc + c_na_n_edif + c_in_n_edif + c_na_n_viv + c_in_n_viv + 
           c_na_piso_n + c_in_piso_n + no_id_unico + c_12_hbt + c_no_upm + 
           c_n_char_upm) %>% 
  filter(c_total > 0) %>% 
  select(-c_total)

a = dim(ocupada)[2]+1
b = dim(inconsistencia)[2]
var_eliminar <- apply(inconsistencia[,a:b],2,sum)[(apply(inconsistencia[,a:b], 2, sum) == 0)]

#incosistencia <- incosistencia %>% select(-c(names(var_eliminar)))
#a = dim(ocupada)[2]+1
#b = dim(incosistencia)[2]
#apply(inconsistencia[,a:b], 2, sum)

inconsistencia_01 <- inconsistencia %>% select(-all_of(names(var_eliminar)))

# -----------------------------------------------------------------------------
# UPM con alto rechazo o nadie en casa
# -----------------------------------------------------------------------------

ocupado_upm <- ocupada %>%
  mutate(
    #primernjh = ifelse(is.na(primernjh)," "," "),
    control_nombre = case_when(substr(primernjh, 1, 5) == "nadie" ~ "Nadie en casa",
                               primernjh == "rechazo" ~ "Rechazo",
                               is.na(primernjh) ~ "Sin nombre",
                               TRUE ~ "Con nombre"),
         # conformidad en el enlistamiento
         re = ifelse(control_nombre %in% c("Con nombre", "Sin Nombre"), 1, 0),
         nr = ifelse(control_nombre == "Rechazo", 1, 0),
         ed = ifelse(control_nombre == "Nadie en casa", 1, 0)) %>% 
  group_by(pro, id_upm) %>% 
  mutate(viv_act = n(),
         viv_re = sum(re),
         viv_nr = sum(nr),
         viv_ed = sum(ed)) %>%
  mutate(viv_nr_ed = (viv_nr + viv_ed)/viv_act) %>% 
  filter(!is.na(id_upm)) %>% 
  left_join(marco, by = c("id_upm","pro")) %>% 
  filter(!is.na(id_upm)) %>% 
  filter(viv_nr_ed > 0.1) %>% 
  group_by(id_upm, pro, can, par, zon, sec, man, loc) %>% 
  summarise(viv_act = n(),
            viv_efectivas = sum(re),
            viv_rechazo = sum(nr),
            viv_ncasa = sum(ed),
            upm_alerta = mean(viv_nr_ed)) %>% 
  mutate(upm_alerta = round(upm_alerta*100, 2))

# -----------------------------------------------------------------------------
# Exportando
# -----------------------------------------------------------------------------

mesm <- paste0(c(rep(2024, 1), rep(2025, 12)), str_pad(c(12:12, 1:12), 2, "left", "0"))
dir <- paste0("intermedios\\03_enlistamiento\\01_concistencia\\",mesm[mese])
dir.create(dir, showWarnings = F) 
dir.exists(dir)

wb <- createWorkbook("Inconsistencias base enlistamiento")
addWorksheet(wb, "Viviendas")
addWorksheet(wb, "UPMs")
writeData(wb, sheet = "Viviendas", inconsistencia_01)
writeData(wb, sheet = "UPMs", ocupado_upm)
  
saveWorkbook(wb, paste0("intermedios/03_enlistamiento/01_concistencia/",mesm[mese], "/", fecha,".xlsx"), overwrite = T)













