
rm(list = ls())

library(srvyr)
source("rutinas/99_librerias/librerias.R")

# ------------------------------------------------------------------------------
# Lectura wk2
#-------------------------------------------------------------------------------

ruta <- "intermedios/05_factores/01_teoricos_ajuste_cobertura/wk2.rds"
wk2 <- readRDS(paste0(ruta))

# ------------------------------------------------------------------------------
# Lectura base personas
#-------------------------------------------------------------------------------

ruta <- "intermedios/04_cobertura/cobertura_base_total_personas.rds"
cobertura_base_total_personas <- readRDS(ruta)


base <- cobertura_base_total_personas %>% 
  left_join(wk2 %>%
              select(id_upm, fexp_teo = d1, fexp_aju = d3), by = "id_upm") %>% 
  mutate( gedad = case_when(edad < 15 ~ 1,
                                   edad >= 15 ~ 1,
                                   T ~ NA),
    id_calib = case_when(pro == "20" ~ "00_9_0_1",
                       T ~ paste0("00", "_", area, "_", sexo, "_", gedad))) %>%  
  # para aumentar id_viv_car en la base
  # left_join(cob_viv %>% 
  #             select(id_upm, panelm, vivienda, id_viv_car), by = c("id_upm", "panelm", "vivienda")) %>% 
  select(id_upm_no_orden, zonal, pro, id_upm, area, estrato,  vivienda, hogar, persona, 
         id_persona, sexo, edad, id_calib, fexp_teo, fexp_aju)

# comprobaciones previas a calibración

colSums(is.na(base))
table(base$id_calib, useNA = "ifany")
sum(base$fexp_teo)
sum(base$fexp_aju)

cat("Número de grupos de calibración (dom_area_sexo_gedad):", "\n", n_distinct(base$id_calib))

# Creación del diseño de muestreo
#
est_pob <- base %>%
  as_survey_design(ids = id_upm,
                   strat = estrato, 
                   weights = fexp_aju,
                   nest = T)
options(survey.lonely.psu="adjust")

sum(weights(est_pob))

# Coeficientes de variación 
est_pob_v <- est_pob %>%
  group_by(id_calib) %>% 
  summarise(n = unweighted(n()),
            var1 = survey_total(vartype = "cv", na.rm = T)) %>% 
  mutate(control = ifelse(var1_cv > 0.10, 1, 0)) %>% 
  print()

cat("CV máximo sobre grupos de calibración:", "\n", paste0(round(max(est_pob_v$var1_cv)*100,2),"%"))
#
# guardar en excel
#
export(est_pob_v, paste0("productos/06_factores/01_mensual/", mes[m], "/cv_id_calib_previo.xlsx"))
saveRDS(base, file = paste0("productos/06_factores/01_mensual/", mes[m], "/personas.rds"))


