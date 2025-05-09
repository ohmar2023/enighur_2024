
source("rutinas/99_librerias/librerias.R")

# -----------------------------------------------------------------------------
# Muestra semana 3 - primer envió
# -----------------------------------------------------------------------------

muestra_sem_3_antigua <- read_excel("productos/02_muestra_usm/periodo_01/muestra_myc.xlsx") %>% 
  mutate(man_nloc = ifelse(zona == "999", "000", manzana),
         id_edif = paste0(provin, canton, parroq, zona, sector, man_nloc, num_edif),
         id_viv = paste0(provin,canton,parroq,zona,sector,man_nloc,num_edif, numviv)) %>% 
  filter(semana %in% c(3)) %>% 
  select(id_upm, provin, canton, parroq,zona,sector,man_nloc,num_edif, numviv,id_edif,id_viv,
         periodo, semana) %>% 
  rename(pro = provin, can = canton, par = parroq, zon = zona, sec = sector,
         n_umce = num_edif)

# -----------------------------------------------------------------------------
# Muestra semana 3 - segundo envio conmpletando todas las UPM 
# -----------------------------------------------------------------------------

muestra_sem_3 <- read_excel("productos/02_muestra_usm/periodo_01/muestra_myc_semana_3.xlsx") %>% 
  mutate(man_nloc = ifelse(zona == "999", "000", manzana),
         id_edif = paste0(provin, canton, parroq, zona, sector, man_nloc, num_edif),
         id_viv = paste0(provin,canton,parroq,zona,sector,man_nloc,num_edif, numviv)) %>% 
  #filter(semana %in%  c(1,2)) %>% 
  select(id_upm, provin, canton, parroq,zona,sector,man_nloc,num_edif, numviv,id_edif,id_viv,
         periodo, semana) %>% 
  rename(pro = provin, can = canton, par = parroq, zon = zona, sec = sector,
         n_umce = num_edif)

# -----------------------------------------------------------------------------
# Cantidad de viviendas que coinciden en ambas bases
# -----------------------------------------------------------------------------

muestra_sem_3 %>% filter(id_viv %in% muestra_sem_3_antigua$id_viv) %>% dim()
dim(muestra_sem_3)

# -----------------------------------------------------------------------------
# Cuantas viviendas de la sem3_nueva se levantaron
# -----------------------------------------------------------------------------

muestra_sem_3 %>% filter(id_viv %in% cobertura_base$id_viv) %>% dim()
dim(muestra_sem_3)

viv_lev_nueva <- muestra_sem_3 %>% filter(id_viv %in% cobertura_base$id_viv) 

# -----------------------------------------------------------------------------
# Cuantas viviendas de la sem3_antigua se levantaron
# -----------------------------------------------------------------------------

muestra_sem_3_antigua %>% filter(id_viv %in% cobertura_base$id_viv) %>% dim()
dim(muestra_sem_3_antigua)

viv_lev_antigua <- muestra_sem_3_antigua %>% filter(id_viv %in% cobertura_base$id_viv)

# -----------------------------------------------------------------------------
# match viviendas levantadas - base nueva y base antigua
# -----------------------------------------------------------------------------

names(viv_lev_antigua)

rbind(viv_lev_antigua,viv_lev_nueva) %>% filter(duplicated(id_viv)) %>% View()








