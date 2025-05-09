
viv_par_ocu_man_sec <- readRDS("D:/OMAR LLAMBO/enighur_2024/insumos/02_muestra_upm/viv_par_ocu_man_sec.rds")

marco_upm <- readRDS("D:/OMAR LLAMBO/enighur_2024/insumos/02_muestra_upm/marco/marco_upm.rds")

v_ciudades_auto <- c("1701","090150","010150","070150","180150",
                     "080150", "230150", "130850", "110150")



marco_upm %>% 
  mutate(zonal = case_when(pro %in% c("04","08","10","17","21","25","30") ~ "norte",
                           pro %in% c("02","05","06","15","16","18","22","29") ~ "centro",
                           pro %in% c("09","12","13","20","23","24","26","32","33") ~ "litoral",
                           pro %in% c("01","03","11","07","14","19","27","28","31") ~ "sur")) %>%
  mutate(cod_ciu = substr(id_upm,1,4)) %>%
  #left_join(viv_par_ocu_man_sec,viv_ocu,id_upm) %>% 
  filter(zonal == "litoral") %>% 
  group_by(cod_ciu,area) %>% 
  summarise( viv_ocupadas = sum(Mi)) %>% 
  arrange(cod_ciu) %>% 
  filter(cod_ciu %in% c("0901","2301","1308")) %>% 
  pivot_wider(names_from = area,values_from = viv_ocupadas) %>% 
  View()
  
