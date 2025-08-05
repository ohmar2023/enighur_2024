
# ------------------------------------------------------------------------------
# Este script solo genera los man_sec de la upm a reemplazar
# Posterior a ello se lo hace mano y se reemplaza en la muestra_upm_man_sec_fondo_rot_006.xlsx
#------------------------------------------------------------------------------

rm(list = ls())

source("rutinas/99_librerias/librerias.R")

# ------------------------------------------------------------------------------
# Cargamos la UPM que ha sido seleccionada para el reemplazo
#------------------------------------------------------------------------------

upm_de_reemplazo <- read_excel("rutinas/99_pedidos/02_reemplazo_upm_001_periodo_11/upm_de_reemplazo.xlsx") %>% 
  mutate(id_conglomerado = substr(id_upm,1,10))

# ------------------------------------------------------------------------------
# Manzanas y sectores
#------------------------------------------------------------------------------

man_sec_upm <- readRDS("insumos/02_muestra_upm/man_sec_upm.rds") %>% 
  rename(id_conglomerado = id_upm)

# ------------------------------------------------------------------------------
# Exportando man sec
#------------------------------------------------------------------------------

man_sec_upm_reemplazo_001 <- upm_de_reemplazo %>% 
  left_join(man_sec_upm, by = "id_conglomerado") %>% 
  select(id_upm, pro, area, estrato, man_sec) %>% 
  mutate(pro = substr(man_sec,1,2),
         can = substr(man_sec,3,4),
         par = substr(man_sec,5,6),
         zon = substr(man_sec,7,9),
         sec = substr(man_sec,10,12),
         man = substr(man_sec,13,15))

rio::export(man_sec_upm_reemplazo_001,
"rutinas/99_pedidos/02_reemplazo_upm_001_periodo_11/man_sec_upm_reemplazo_001.xlsx")



