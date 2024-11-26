
rm(list = ls())

{
  library(rio)
  library(tidyverse)
  library(janitor)
  library(readxl)
  library(openxlsx)
}

# -----------------------------------------------------------------------------
# Lectura base enlistamiento, muestra y novedades
# -----------------------------------------------------------------------------

base <- readRDS("intermedios/03_enlistamiento/01_concistencia/base.rds")
muestra <- import("muestra_upm_man_sec_fondo_rot_004.xlsx")
novedades <- read_excel("insumos/03_enlistamiento/2024_11_25/novedades_j1.xlsx") %>% 
  clean_names() %>% mutate(id_upm = upm) 

per <- 1
mese <- 1

# -----------------------------------------------------------------------------
# mansec nuevos 
# -----------------------------------------------------------------------------

man_sec_nuevos <- muestra %>% filter(periodo_nuevo == per) %>% 
  full_join(select(base,man_sec = man_sec_21,id_upm),
            by = c("man_sec","id_upm")) %>% 
  filter(is.na(zonal)) %>% 
  select(id_upm,man_sec) %>% 
  group_by(id_upm,man_sec) %>% 
  summarise() 

# -----------------------------------------------------------------------------
# mansec en la muestra que no aparecen en el enlistamiento
# -----------------------------------------------------------------------------

man_sec_no_enlistados <- muestra %>% filter(!man_sec %in% base$man_sec_21, periodo_nuevo==1) %>% 
  select(id_upm,man_sec) %>% 
  group_by(id_upm,man_sec) %>% 
  summarise() 

# -----------------------------------------------------------------------------
# upm no enlistadas
# -----------------------------------------------------------------------------

upm_no_enlistadas <- muestra %>% filter(!id_upm %in% base$id_upm, periodo_nuevo==1) %>% 
  select(id_upm,man_sec) %>% 
  group_by(id_upm) %>% 
  summarise()

# -----------------------------------------------------------------------------
# upm que no aparecen en las novedades y no estan enlistadas
# -----------------------------------------------------------------------------

muestra %>% filter(!id_upm %in% base$id_upm, periodo_nuevo==1) %>% 
  select(id_upm,man_sec) %>% 
  group_by(id_upm) %>% 
  summarise() %>% 
  filter(!id_upm %in% novedades$id_upm) %>% 
  View()

# -----------------------------------------------------------------------------
# Exportando
# -----------------------------------------------------------------------------


mesm <- paste0(c(rep(2024, 1), rep(2025, 12)), str_pad(c(12:12, 1:12), 2, "left", "0"))
dir <- paste0("intermedios\\03_enlistamiento\\01_concistencia\\",mesm[mese])
#dir.create(dir, showWarnings = F) 
dir.exists(dir)

wb <- createWorkbook("Inconsistencias base enlistamiento MANSEC-UPM")
addWorksheet(wb, "man_sec_nuevos")
addWorksheet(wb, "man_sec_no_enlistados")
addWorksheet(wb, "upm_no_enlistadas")

writeData(wb, sheet = "man_sec_nuevos", man_sec_nuevos)
writeData(wb, sheet = "man_sec_no_enlistados", man_sec_no_enlistados)
writeData(wb, sheet = "upm_no_enlistadas", upm_no_enlistadas)

saveWorkbook(wb, paste0("intermedios/03_enlistamiento/01_concistencia/",mesm[mese], "/", "incon_man_sec_upm_",mesm[mese],".xlsx"), overwrite = T)


