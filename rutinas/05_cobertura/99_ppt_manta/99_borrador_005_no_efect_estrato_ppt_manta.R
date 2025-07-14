

marco_upm <- readRDS("insumos/02_muestra_upm/marco/marco_upm.rds")

no_efectivas_estrato <- cobertura_base_total %>% 
  left_join(select(marco_upm, id_upm,  estrato), by = "id_upm") %>% 
  mutate(estrato_2 = substr(estrato, 4, 4)) %>%
  group_by(zonal, n_rvo_aux, area, estrato_2) %>% 
  summarise(n = n()) %>% 
  filter(n_rvo_aux == "No efectiva") %>% 
  group_by(zonal, n_rvo_aux, area) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(porc_n = n / total) %>%  
  pivot_wider(id_cols = -n, names_from = estrato_2, 
              values_from = porc_n)

rio::export(no_efectivas_estrato, 
            "documentos/comisión manta/ok_no_efectivas_estrato_viv.xlsx")





