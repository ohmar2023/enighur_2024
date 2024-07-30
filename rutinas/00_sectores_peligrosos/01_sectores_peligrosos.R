
rm(list = ls())

{
  library(sf)
  library(sp) 
  library(rio)
  library(tidyverse)
  library(janitor)
  library(leaflet)
  library(plotly)
}

# MARCO UPM --------------------------------------------------------------------
marco_upm <- readRDS("insumos/02_muestra_upm/marco/marco_upm.rds")
# -------------------------------------------------------------------------
# LECTURA DE LOS SHAPES A NIVE DE SECTORES Y MANZANAS ---------------------
# -------------------------------------------------------------------------

sectores <- read_sf("insumos/00_geo_shapes/BNCPV22.gpkg", layer = "sec_a")
manzanas <- read_sf("insumos/00_geo_shapes/BNCPV22.gpkg", layer = "man_a")

names(sectores)

sectores %>% 
  st_drop_geometry() %>% 
  View("sectores")

manzanas %>% 
  st_drop_geometry() %>% 
  View("manzanas")

# -------------------------------------------------------------------------
# MACHALA - PELIGROSOS ----------------------------------------------------
# -------------------------------------------------------------------------

machala_peligroso <- c("070150001001",
                       "070150001701",
                       "070150002401",
                       "070150005601",
                       "070150016201",
                       "070150016401",
                       "070150022201",
                       "070150032201",
                       "070150044001",
                       "070150044201",
                       "070150045501",
                       "070150045601",
                       "070150046401",
                       "070150071101",
                       "070150071201",
                       "070150071601",
                       "070150078401",
                       "070150085401",
                       "070150089401")

machala_zonas <- c("070150002",
                   "070150003",
                   "070150004",
                   "070150005",
                   "070150006",
                   "070150007",
                   "070150008",
                   "070150009",
                   "070150015",
                   "070150016",
                   "070150017",
                   "070150064",
                   "070150066")

sectores %>% 
  mutate(pro = substr(sec,1,6),
         zonas = substr(sec,1,9),
         quitar = case_when(sec %in% machala_peligroso ~ "PELIGROSO",
                            zonas %in% machala_zonas ~ "PELIGROSO",
                            T ~ "SE PUEDE LEVANTAR")) %>% 
  filter(pro == "070150") %>% 
  ggplot() +
  geom_sf(aes(fill = quitar)) +
  ggtitle("Machala (Parroquia)") +
  labs(fill = "Zonas")


marco_upm %>% 
  group_by(pro) %>% 
  mutate(total_viv = sum(Mi)) %>% 
  ungroup() %>% 
  filter(id_upm %in% machala_peligroso) %>% 
  group_by(pro) %>% 
  summarise(prom = sum(Mi)*100/unique(total_viv)) %>% 
  View()
 
# -------------------------------------------------------------------------
# MORONA - COSTOSO --------------------------------------------------------
# -------------------------------------------------------------------------


morona_costosos <- c("140950999003",
                     "140950999004",
                     "140950999005",
                     "140950999006",
                     "140950999007",
                     "140950999008",
                     "140950999009",
                     "140950999010",
                     "140950999012",
                     "140950999013",
                     "140951999001",
                     "140951999002",
                     "140951999003",
                     "140951999004",
                     "140951999005",
                     "140951999006",
                     "140951999007",
                     "140951999008",
                     "140951999009",
                     "140951999010",
                     "140951999011",
                     "140952999002",
                     "140952999003",
                     "140952999004",
                     "140952999005",
                     "140952999006",
                     "140952999007",
                     "140952999008",
                     "140952999009",
                     "140952999010",
                     "140952999011",
                     "140952999012",
                     "140952999013",
                     "140952999014",
                     "140952999015",
                     "140952999016",
                     "140952999017",
                     "140952999018",
                     "140952999019",
                     "140952999020",
                     "140952999021",
                     "140952999022",
                     "140952999023",
                     "140953999025",
                     "140953999027",
                     "140953999028",
                     "140953999030",
                     "140953999031",
                     "140953999032",
                     "140953999033",
                     "140954001001",
                     "140954999001",
                     "140954999002",
                     "140954999003",
                     "140954999004",
                     "140954999005",
                     "140954999006",
                     "140954999007",
                     "140954999008",
                     "140954999009",
                     "140954999010",
                     "140954999011",
                     "140954999012",
                     "140954999013",
                     "140954999014",
                     "140954999015",
                     "140954999016",
                     "140954999017",
                     "140954999018",
                     "140954999019",
                     "140954999020",
                     "140954999021",
                     "140954999022",
                     "140954999023",
                     
                     "140950900101",
                     "140951900201",
                     "140951900401",
                     "140952900101",
                     "140953900801",
                     "140953901301",
                     "140954900601")


sectores %>% 
  mutate(pro = substr(sec,1,2),
         zonas = substr(sec,1,9),
         quitar = case_when(sec %in% morona_costosos ~ "COSTOSO",
                            T ~ "SE PUEDE LEVANTAR")) %>% 
  filter(pro == "14") %>% 
  ggplot() +
  geom_sf(aes(fill = quitar)) +
  ggtitle("Morona Santiago (Provincia)")+
  labs(fill = "Zonas")


# -------------------------------------------------------------------------
# PASTAZA - PELIGROSOS ----------------------------------------------------
# -------------------------------------------------------------------------

pastaza_costosos <- c("160162999023",
                      "220159999002",
                      "220159999003")

pastaza_costosos_parr <- c("160156",
                           "160158",
                           "160159",
                           "160161",
                           "160451")
sectores %>% 
  mutate(pro = substr(sec,1,2),
         parr = substr(sec,1,6),
         quitar = case_when( sec %in% pastaza_costosos ~ "PELIGROSO",
                             parr %in% pastaza_costosos_parr ~ "PELIGROSO",
                             T ~ "SE PUEDE LEVANTAR")) %>% 
  filter(pro == "16") %>% 
  ggplot() +
  geom_sf(aes(fill = quitar))+
  ggtitle("Pastaza (Provincia)")+
  labs(fill = "Zonas")

# -------------------------------------------------------------------------
# ORELLANA - COSTOSOS -----------------------------------------------------
# -------------------------------------------------------------------------

orellana_costosos <- c("220159999002",
                      "220159999003")

sectores %>% 
  mutate(pro = substr(sec,1,6),
         zonas = substr(sec,1,9),
         quitar = case_when(sec %in% orellana_costosos ~ "COSTOSO",
                            T ~ "SE PUEDE LEVANTAR")) %>% 
  filter(pro == "220159") %>% 
  ggplot() +
  geom_sf(aes(fill = quitar)) + 
  ggtitle("NUEVO PARAISO (Parroquia) - Orellana")+
  labs(fill = "Zonas")



# -------------------------------------------------------------------------
# VVIIENDAS DESCARTADAS ---------------------------------------------------
# -------------------------------------------------------------------------
man_sec_upm <- man_sec_upm %>% 
  mutate(sec = substr(man_sec,1,12))

v_t <- c(machala_peligroso,morona_costosos)

length(v_t)

aux <- man_sec_upm %>% filter(sec %in% v_t) 
n_distinct(aux$sec)

aux %>% 
  group_by(sec) %>%
  summarise(sum(viv_ocu)) %>% 
  adorn_totals() %>% 
  View()
  
aux %>% 
  mutate(pro = substr(sec,1,2)) %>% 
  group_by(pro) %>%
  summarise(sum(viv_ocu)) %>% 
  adorn_totals() %>% 
  View()














