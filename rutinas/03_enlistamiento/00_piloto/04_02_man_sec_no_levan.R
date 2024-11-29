
# -----------------------------------------------------------------------------
# Lectura bases DICA
# -----------------------------------------------------------------------------

repor_cober <- read_excel("insumos/03_enlistamiento/2024_11_28/ReportedeCobertura_2024-11-25_11_25ParaDinem.xlsx") %>% 
  clean_names() %>% 
  filter(tipo_de_formulario != "Generado")

upm_inc <- read_excel("insumos/03_enlistamiento/2024_11_28/incon_man_sec_upm_202412REvisadoCartografia.xlsx",
                      sheet = "upm_no_enlistadas")

# -----------------------------------------------------------------------------
# Validacion mansec en la muestra no levantadas
# -----------------------------------------------------------------------------

man_sec_no_enlistados %>% 
  filter(!id_upm %in% upm_inc$id_upm) %>% 
  mutate(revision = ifelse(man_sec %in% repor_cober$clave_muestra_man_sec,1,0)) %>% 
  filter(revision == 0) %>% 
  View()

# -----------------------------------------------------------------------------
# Validacion mansec nuevos
# -----------------------------------------------------------------------------
man_sec_nuevos %>% 
  mutate(revision = ifelse(man_sec %in% repor_cober$clave_captura,1,0)) %>% 
  filter(revision == 0) %>% 
  View()

# -----------------------------------------------------------------------------
# Los que sobran del reporte
# -----------------------------------------------------------------------------

repor_cober %>% 
  mutate(revision = ifelse(clave_captura %in% man_sec_nuevos$man_sec,1,0)) %>% 
  filter(revision == 0) %>% 
  View()



n_distinct(repor_cober$clave_captura)
n_distinct(man_sec_nuevos)
repor_cober %>% filter(tipo_de_formulario != "Generado") %>% dim()

export(aux,"man_sec_no_enlistados_revision.xlsx")
