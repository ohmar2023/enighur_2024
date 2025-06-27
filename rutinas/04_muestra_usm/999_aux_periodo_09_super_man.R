# Este script se generó para el periodo 07:
# La novedad surge en el coglomerado (10 dig) 1101500063 que es una supermanzana
# y tiene dos UPM (110150006302 & 110150006301) y estas dos UPM fueron seleccioandas, 
# ambas tienen los mismos man_sec. En la base alfanumerica de cartografia no 
# aparece la UPM 110150006302 y como ambas tienen los mismo man_sec en su base
# ellos solo hacen constar la UPM 110150006301, por lo que yo tengo "perdida"
# la otra UPM.
# Adicional a esto, dos de las tres manzanas (man_sec de estas UPM) fueron unidas
# por ello toca generar una nueva partición de edificios y asignar a cada UPM, 
# esto implica "crear" la UPM 110150006302 en la base "base_ocupada". Para la partición
# Se consideró la mitad de viviendas de la man 004 (n_umce <= "114") y así va
# una mitad a cada UPM.
# Las viv de la man 003 también van a la UPM "110150006301".
# Luego hay que corregir la semana de levantamiento de cada UPM.
# Luego solo se agrega el grupo "a mano" en base a la partición que se hizo ya. 

# ------------------------------------------------------------------------------
# Correr después de la linea 28 del script 01_seleccion_muestra_usm.R
# ------------------------------------------------------------------------------

base_ocupada <- base_ocupada %>% 
  mutate(id_conglomerado = substr(id_upm,1,10)) %>% 
  arrange(man, n_umce) %>% 
  mutate(id_upm = case_when(id_conglomerado == "1308500216" & n_umce <= "200" ~ "130850021601", 
                            id_conglomerado == "1308500216" & n_umce > "200" ~ "130850021602", 
                            T ~ id_upm), 
         semana_nueva = case_when(id_upm == "130850021601" ~ 33, 
                                  id_upm == "130850021602" ~ 35, 
                                  T ~ semana_nueva)) %>%
  select(-id_conglomerado)


# ------------------------------------------------------------------------------
# Correr después de la linea 122 del script 01_seleccion_muestra_usm.R
# ------------------------------------------------------------------------------

marco_viv_superman <- marco_viv_superman %>% 
  mutate(grupo = case_when(id_upm == "180150034202" & man == "003" ~ 1, 
                           id_upm == "180150034202" & man == "004" ~ 2, 
                           T ~ grupo))

# ------------------------------------------------------------------------------
# Correr después de la linea 122 del script 01_seleccion_muestra_usm.R
# ------------------------------------------------------------------------------

marco_viv_superman <- marco_viv_superman %>% 
  mutate(grupo = case_when(id_conglomerado == "1308500216" & n_umce <= "200" ~ 1, 
                           id_conglomerado == "1308500216" & n_umce > "200" ~ 2, 
                           T ~ grupo))





