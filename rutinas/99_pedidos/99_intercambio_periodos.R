

muestra <- import("productos/02_muestra_upm/muestra_upm_man_sec_fondo_rot_008.xlsx")
marco_upm <- readRDS("insumos/02_muestra_upm/marco/marco_upm_23.rds")


aux <- muestra %>% 
  group_by(id_upm, periodo_nuevo) %>% 
  summarise() %>% 
  left_join(select(marco_upm, id_upm, estrato)) %>% 
  filter(periodo_nuevo>=11)


###############################################################################
v_01 <- c("100450005901", "100250004301")
aux %>% filter(id_upm %in% v_01)
###############################################################################

v_02 <- c("100350902301", "080150060701")
aux %>% filter(id_upm %in% v_02)


v_03 <- c("100451001201", "080150007601")
aux %>% filter(id_upm %in% v_03)
