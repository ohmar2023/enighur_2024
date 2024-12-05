
muestra_myc <- read_excel("productos/02_muestra_usm/muestra_myc_enviada.xlsx") %>% 
  filter(periodo == 1)

table(muestra_myc$ndominio)

muestra_myc <- muestra_myc %>% 
  mutate(ndominio = gsub(ndominio,pattern = "_",replacement = " "),
         calle = gsub(calle,pattern = "°",replacement = " "),
         calle = gsub(calle,pattern = "\\(|\\)",replacement = ""),
         calle = gsub(calle,pattern = "\\,|\\.",replacement = ""),
         numnum = gsub(numnum,pattern = "/" ,replacement = ""),
         nparroq = gsub(nparroq,pattern = "EL PARAISO LA 14",
                        replacement = "EL PARAISO LA CATORCE"))

#export(muestra_myc, "productos/02_muestra_usm/muestra_myc.xlsx")

grep(muestra_myc$calle,pattern = ",|.")
sum(is.na(muestra_myc$calle))

# -----------------------------------------------------------------------------
# Lectura bases DICA
# -----------------------------------------------------------------------------


muestra_myc_antigua <- read_excel("productos/02_muestra_usm/cambios/muestra_myc_correcciones_myc.xlsx")
muestra_myc_nueva <- read_excel("productos/02_muestra_usm/muestra_myc.xlsx") 

dim(muestra_myc_antigua) == dim(muestra_myc_nueva)

apply(muestra_myc_antigua == muestra_myc_nueva, 2, sum)

sum(muestra_myc_antigua$manzana == muestra_myc_nueva$manzana)
table(muestra_myc_antigua$manzana,useNA = "ifany")
table(muestra_myc_nueva$manzana,useNA = "ifany")




muestra <- import("muestra_upm_man_sec_fondo_rot_004.xlsx") %>% 
  filter(periodo_nuevo==1) %>% group_by(id_upm,semana_nueva) %>% summarise()

n_distinct(muestra_myc$id_upm)
n_distinct(muestra$id_upm)

table(muestra$semana_nueva)
table(muestra_myc$semana)

muestra %>% filter(!id_upm %in% muestra_myc$id_upm) %>% View()

muestra_myc %>% filter(zonal == "LITORAL") %>% group_by(ndominio,dominio) %>% summarise()
unique(muestra_myc$zonal)

aux <- muestra_myc_nueva %>% group_by(id_upm) %>% summarise(semana = unique(semana))
table(muestra_myc_nueva$semana)
dim(muestra_myc)













