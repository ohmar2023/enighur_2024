#
rm(list = ls())

library(rio)
library(tidyverse)
library(srvyr)

# cargamos base de cobertura
mes <- list.files("productos/05_cobertura")
mes
m <- max(length(mes))
mes[m]

load(paste0("intermedios/05_cobertura/", mes[m], "/campo.RData"))
load(paste0("productos/06_factores/01_mensual/", mes[m], "/wk.RData"))

rm(caratula, muestra, wk0, control)

# 1) creación de variables id_calib
#
base <- personas %>% 
  mutate(gedad = case_when(edad < 15 ~ 1,
                           edad >= 15 ~ 2,
                           T ~ NA),
         id_calib = case_when(pro == "20" ~ "00_9_0_1",
                              T ~ paste0("00", "_", area, "_", sexo, "_", gedad))) %>% 
  left_join(wk2 %>%
              select(id_upm, fexp_teo = d1, fexp_aju = d3), by = "id_upm") %>% 
  # para aumentar id_viv_car en la base
  # left_join(cob_viv %>% 
  #             select(id_upm, panelm, vivienda, id_viv_car), by = c("id_upm", "panelm", "vivienda")) %>% 
  select(id_viv_car, zonal, pro, id_upm, area, estrato, panelm, vivienda, hogar, persona, id_per,
         mes, sexo, edad, n0a14, n15a99, p06, gedad, id_calib, fexp_teo, fexp_aju)

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


