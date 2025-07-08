rm(list = ls())

source("rutinas/99_librerias/librerias.R")
source("rutinas/99_librerias/read_zip.R")

# -----------------------------------------------------------------------------
# Lectura de la base de cobertura: Necesitamos indicar el periodo
# -----------------------------------------------------------------------------

periodo <- 5
periodo <- str_pad(periodo,2,"left","0")

# -----------------------------------------------------------------------------
# Lectura de la bases de cobertura de cada periodo
# -----------------------------------------------------------------------------

cobertura_base_total <- NULL

for(i in c(1:as.numeric(periodo))){
  periodo <- str_pad(i,2,"left","0")
  direc = paste0("intermedios/04_cobertura/periodo_", periodo,"/cobertura_base_periodo_", periodo, ".rds")
  #archivo_zip = dir(direc)[grep(dir(direc), pattern = "v1_")]
  #documento = "IDENTIFICACION"
  
  aux <- import(direc)
  
  cobertura_base_total <- rbind(cobertura_base_total,aux)
}

# -----------------------------------------------------------------------------
# 
# -----------------------------------------------------------------------------

# cobertura_base_total %>% group_by(paste0(id_upm,vivienda)) %>% 
#   summarise(n = n()) %>% 
#   filter(n>1) %>% 
#   View("ojo")

cobertura_base_total_1 <- cobertura_base_total %>% 
  filter(ifelse(hogar == 5 & rvo != 1, FALSE, TRUE)) %>%
  mutate(id_upm_no_orden = paste0(id_upm, vivienda)) %>% 
  mutate(n_rvo = case_when(rvo == 1 ~ "Completa",
                           rvo == 2 ~ "Rechazo",
                           rvo == 3 ~ "Nadie en casa",
                           rvo == 4 ~ "Vivienda temporal",
                           rvo == 5 ~ "Vivienda desocupada",
                           rvo == 6 ~ "Vivienda en construcción",
                           rvo == 7 ~ "Vivienda inhabitada o destruida",
                           rvo == 8 ~ "Vivienda convertida en negocio",
                           rvo == 9 ~ "Otra razón",
                           rvo == 10 ~ "Rechazo a mitad de la encuesta")) %>% 
  mutate(Elegibilidad = case_when(rvo == 1 ~ "re",
                                  rvo == 2 ~ "nr",
                                  rvo %in% c(4,5,6,7,8,9) ~ "ne",
                                  rvo == 3 ~ "ed",
                                  TRUE ~ "error")) %>% 
  group_by(id_upm_no_orden) %>% 
  mutate(revision_1 = 1, 
         aux_1 = row_number(),
         aux_2 = n(),
         revision_2 = ifelse (aux_2 > 1 & n_rvo != "Completa" , 0, revision_1)) %>% 
  ungroup() %>% 
  filter(revision_2 == 1) %>% 
  filter(!duplicated(id_upm_no_orden))


# a <- cobertura_base_total %>% group_by(pro, Elegibilidad) %>% summarise(n_ori = n())
# b <- cobertura_base_total_1 %>% group_by(pro, Elegibilidad) %>% summarise(n_1 = n())
# 
# a %>% left_join(b) %>% 
#   mutate(diferencia = n_1 - n_ori) %>% 
#   filter(pro == "17") %>% 
#   adorn_totals() %>% 
#   View()

# lol <- cbind(table(cobertura_base_total$rvo), 
#       table(cobertura_base_total_1$rvo)) %>% data.frame() %>% 
#   mutate(diferencia = X1 - X2)

cobertura_base_total <- cobertura_base_total_1 %>% 
  select(-aux_1, -aux_2, -revision_1, -revision_2)
# cobertura_base_total_1 %>% group_by(id_upm_no_orden) %>% summarise(n()) %>% View("ojo")
# 
# cobertura_base_total_1 %>% select(id_upm, id_upm_no_orden,
                                # vivienda, hogar, n_rvo, aux_1, 
                                # aux_2, revision_1, revision_2) %>% View()

# cobertura_base_total_1 %>% group_by(id_upm_no_orden, n_rvo) %>%
#   summarise(n = n()) %>% 
#   pivot_wider(names_from = n_rvo, values_from = n) %>% 
#   adorn_totals(c("col")) %>% 
#   filter(Total > 1) %>% 
#   adorn_totals(c("row")) %>% 
#   View()
  
# -----------------------------------------------------------------------------
# Lectura de la bases de cobertura de cada periodo
# -----------------------------------------------------------------------------

ruta <- paste0("intermedios/04_cobertura/")
rio::export(cobertura_base_total, 
       paste0(ruta,"cobertura_base_total.rds"), overwrite = FALSE)



