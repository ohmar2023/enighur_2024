
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
