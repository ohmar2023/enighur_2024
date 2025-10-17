
rm(list = ls())

source("rutinas/99_librerias/librerias.R")
source("rutinas/99_librerias/tot_pob.R")

mes <- "a25m06" 

# ------------------------------------------------------------------------------
# Lectura base de personas
#-------------------------------------------------------------------------------

ruta <- "intermedios/05_factores/02_calibracion/"
base <- import(paste0(ruta,"personas.rds"))

#-------------------------------------------------------------------------------
# Creación de variables id_calib
#-------------------------------------------------------------------------------

cat("Número de grupos de calibración (dom_area_sexo_gedad):", "\n", n_distinct(base$id_calib))

table(base$id_calib, useNA = "ifany")

#
# poblaciones objetivo para la calibracion
#
pob_nac <- tot_pob(dominio = "prov", si.area = T, si.sexo = T, gedad = c(0,99), anio = mes) |> 
  filter(area != 9)

pob_gal <- tot_pob(dominio = "prov", si.area = T, si.sexo = F, gedad = c(0,99), anio = mes) |> 
  filter(area == 9)

pob <- rbind(pob_nac, pob_gal) |> 
  mutate(id_calib = paste(dominio, area, sexo, gedad, sep = "_")) |> 
  select(id_calib, t)

rm(pob_nac, pob_gal)
print(sum(pob$t))
 
#
# totales estimados por Horvitz-Thompson por dominio y/o area y/o sexo
#
tp <- base |> 
  group_by(id_calib) |> 
  summarise(d = sum(fexp_aju))

# comprobaciones: ver tp y t
vis <- tp |> 
  left_join(pob, by = "id_calib") |> 
  mutate(dif = t - d) |> 
  arrange(id_calib)|>
  mutate(cotas = t/d)

# Gráfica: lo expandido (d) vs. las proyecciones (t)
ylim <- c(1.25 * min(vis$dif), 1 * max(vis$dif))

barplot(vis$dif , border=F , names.arg = vis$id_calib, 
        las = 2 , 
        col = c("darkgreen", "bisque", "darkorange",  "darkorange") , 
        ylim = ylim , 
        main = "Diferencia = Expandido - Proyecciones"
)

#rm(pob, tp)

#-------------------------------------------------------------------------------
# Calibración de hogar integrado

# apoyo_Cal: Los id_calibs como variables y llenas de 1 y 0. Igual que base.
#-------------------------------------------------------------------------------

apoyo_cal <- base |>
  select(id_upm, vivienda, hogar, persona,
         id_calib, estrato, fexp_aju) |>
  mutate(n = 1,
         id_calib = paste0("cx", id_calib)) |>
  pivot_wider(names_from = id_calib, values_from = n, values_fill = 0) |> 
  ungroup()

aux <- base |>
  select(id_upm, vivienda, hogar, 
         id_calib, estrato, fexp_aju) |> # se utiliza el factor ajustado por cobertura
  group_by(id_upm, vivienda, hogar) |> 
  mutate(totper = n()) |> #Solo cuento las personas en cada vivienda
  group_by(id_upm, vivienda, hogar, estrato, fexp_aju, id_calib) |> 
  summarise(totper = mean(totper),
            n = n()) |> 
  mutate(prom = n / totper,
         id_calib = paste0("cx", id_calib)) |>
  select(-n) |>
  pivot_wider(names_from = id_calib, values_from = prom, values_fill = 0) |> 
  ungroup() |>
  select(-c(estrato, fexp_aju, totper))

base_hi <- apoyo_cal |>
  select(id_upm, vivienda, hogar, persona, estrato, fexp = fexp_aju) |>
  left_join(aux, by = c("id_upm",  "vivienda", "hogar"))

rm(apoyo_cal, aux)

diseno_hi <- base_hi |> 
  left_join(base |> 
              select(id_upm,  vivienda, hogar, persona, id_calib),
            by = c("id_upm", "vivienda", "hogar", "persona")) |> 
  as_survey(ids = id_upm,
            strata = estrato,
            weights = fexp,
            nest = TRUE)

#-------------------------------------------------------------------------------
# Diseño de calibración
#-------------------------------------------------------------------------------

totales <- setNames(vis$t, paste0("cx", vis$id_calib))
totales

var_independientes <- names(base_hi)[grepl("cx", names(base_hi))]
var_independientes <- var_independientes[order(match(var_independientes, paste0("cx", vis$id_calib)))]
var_independientes

mc <- as.formula(
  paste("~", 0, "+", paste(var_independientes, collapse = " + "))
)

calib_hi <- diseno_hi |> 
  calibrate(formula = mc,
            population = totales,
            bounds = c(0.1, Inf))

#-------------------------------------------------------------------------------
# Estimaciones
#-------------------------------------------------------------------------------

#diseño hogar
estimado_cal_hi <- calib_hi |>
  group_by(id_calib) |> 
  summarise(survey_total()) |> 
  data.frame()

estimado_cal_hi
vis

summary(weights(calib_hi))

hist(weights(calib_hi), 30)

#-------------------------------------------------------------------------------
# Generar base a entregar.
#-------------------------------------------------------------------------------

base2 <- data.frame(id_upm = diseno_hi$variables$id_upm,
                    vivienda = diseno_hi$variables$vivienda,
                    hogar = diseno_hi$variables$hogar,
                    persona = diseno_hi$variables$persona,
                    estrato = diseno_hi$variables$estrato,
                    fexp = diseno_hi$variables$fexp,
                    fexp_cal = weights(calib_hi)) |> 
  left_join(base |> 
              select(id_upm, vivienda, hogar, persona,
                     pro, area, sexo, edad, id_calib),
            by = c("id_upm",  "vivienda", "hogar", "persona")) |> 
  select(id_upm,  vivienda, hogar, persona, estrato,
         id_calib, pro, area, sexo, edad, fexp, fexp_cal)

colSums(is.na(base2))

# guardado






























# Verificacion que el fexp calibrado sea unico por hogar
verific <- calibracion_hog_int |>
  group_by(id_upm, mes, vivienda, hogar) |>
  summarise(n_fexp = n_distinct(fexp_cal_hi))

table(verific$n_fexp, useNA = "ifany")

# emparejar factores en bases 

base_fexp <- base |> 
  full_join(calibracion_hog_int,
            by = c("id_upm", "mes", "panelm", "vivienda", "hogar", "persona"))

print("Comprobación dimensiones")
print(dim(base)[1] == dim(base_fexp)[1])

print("Totales poblacionales estimados")
print(sum(base_fexp$fexp_teo))
print(sum(base_fexp$fexp_aju))
print(sum(base_fexp$fexp_cal_hi))
print(sum(vis$t))

# Verificacion de las poblaciones
vis <- base_fexp |>
  group_by(id_calib) |>
  summarise(pob_hog_int = sum(fexp_cal_hi)) |> 
  ungroup |> 
  full_join(vis, by = "id_calib") |>
  mutate(dif_pob_hog_int = t - pob_hog_int) |> 
  select(id_calib, pp = t, est_previa = d, dif_previa = dif, cotas, est_cal = pob_hog_int, dif_cal = dif_pob_hog_int)

print(paste0("Diferencia máxima sobre grupos de calibración hogar: ",
             print(max(abs(vis$dif_cal)))))
vis

#
# Guardar las bases
#
base_entrega <- base_fexp |> 
  mutate(fexp_nor = fexp_aju/sum(fexp_aju)) |> 
  select(id_viv_car, zonal, pro, area, estrato, id_upm, panelm, vivienda, hogar, persona, id_per,
         mes, id_calib, fexp_cal_hi, fexp_nor) 

saveRDS(base_fexp, file = paste0("productos/06_factores/01_mensual/", mes[m], "/personas_fexp.rds"))
saveRDS(base_entrega, file = paste0("productos/06_factores/01_mensual/", mes[m], "/personas_fexp_", mes[m], ".rds"))
export(vis, paste0("productos/06_factores/01_mensual/", mes[m], "/vis.xlsx"))


