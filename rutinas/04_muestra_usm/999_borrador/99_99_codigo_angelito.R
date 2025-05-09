
a <- readRDS("insumos/99_marco_viv_precenso/marco_viv_ocu_nap.rds") %>% 
  mutate(id_conglomerado = substr(id_upm,1,10)) %>% 
    filter(id_conglomerado %in% upm_super_man$id_conglomerado)

b <- base_ocupada %>% 
  mutate(id_conglomerado = substr(id_upm,1,10),
         n_upm_sel = substr(id_upm,12,12)) %>% 
  filter(id_conglomerado %in% upm_super_man$id_conglomerado)   

b %>% left_join(select(a,id_conglomerado,id_viv = id_vivienda, n_umce = n_edif, id_upm, pro), 
                by = c("id_conglomerado","id_upm","n_umce")) %>% 
   filter(id_conglomerado == "1701500367", is.na(pro.y)) %>% View()

a %>% filter(id_conglomerado == "1701500367") %>% dim()
b %>% filter(id_conglomerado == "1701500367") %>% dim()

unique(b$id_conglomerado)

export(a,"a.xlsx")
export(b,"b.xlsx")


base %>% filter(man_sec_21=="170150315003001") %>% View()

# -----------------------------------------------------------------------------

particion_manzanas_li_60 <- read_rds("insumos/99_supermanzanas/particion_manzanas_li_60.rds")

b <- base_ocupada %>% 
  mutate(id_conglomerado = substr(id_upm,1,10),
         n_upm_sel = substr(id_upm,12,12)) %>% 
  filter(id_conglomerado %in% upm_super_man$id_conglomerado) %>% 
  left_join(select(particion_manzanas_li_60,id_edif,grupo),by= "id_edif") %>% 
  filter(grupo == n_upm_sel)

table(b$grupo,useNA = "ifany")

dim(b)



# ----------------------------------------------------------------------------



table(base_ocupada$celular, useNA = "ifany")

sum(is.na(base_ocupada$celular))
dim(base_ocupada)

sum(is.na(base_ocupada$convencional))
dim(base_ocupada)














