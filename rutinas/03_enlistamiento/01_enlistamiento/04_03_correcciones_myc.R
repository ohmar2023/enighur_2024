
muestra_myc <- read_excel("productos/02_muestra_usm/muestra_myc_enviada.xlsx")

table(muestra_myc$ndominio)

muestra_myc <- muestra_myc %>% 
  mutate(ndominio = gsub(ndominio,pattern = "_",replacement = " "),
         calle = gsub(calle,pattern = "°",replacement = " "),
         calle = gsub(calle,pattern = "\\(|\\)",replacement = ""),
         calle = gsub(calle,pattern = "\\,|\\.",replacement = ""),
         numnum = gsub(numnum,pattern = "/" ,replacement = ""),
         nparroq = gsub(nparroq,pattern = "EL PARAISO LA 14",
                        replacement = "EL PARAISO LA CATORCE"))

export(muestra_myc, "productos/02_muestra_usm/muestra_myc.xlsx")

grep(muestra_myc$calle,pattern = ",|.")
sum(is.na(muestra_myc$calle))

# -----------------------------------------------------------------------------
# Lectura bases DICA
# -----------------------------------------------------------------------------


muestra_myc <- read_excel("productos/02_muestra_usm/muestra_myc.xlsx") %>% 
  filter(periodo==1) %>%  group_by(id_upm,semana) %>% summarise()

muestra <- import("muestra_upm_man_sec_fondo_rot_004.xlsx") %>% 
  filter(periodo_nuevo==1) %>% group_by(id_upm,semana_nueva) %>% summarise()

n_distinct(muestra_myc$id_upm)
n_distinct(muestra$id_upm)

table(muestra$semana_nueva)
table(muestra_myc$semana)

muestra %>% filter(!id_upm %in% muestra_myc$id_upm) %>% dim()







