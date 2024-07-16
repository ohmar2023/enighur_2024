
{
  library(tidyverse)
  library(janitor)
  library(rio)
}

dis_galapagos <- distribucion %>% 
  mutate(islas = substr(id_upm, 1,4),
         pro = substr(id_upm, 1,2),
         islas_2 = substr(id_upm, 1,6)) %>% 
  filter(pro == "20") %>% arrange(islas,1)

# san_crsitobal ---------------------------------------------------------------
san_crsitobal = dis_galapagos %>% 
  filter(islas == "2001" & islas_2 != "200152")

# floreana --------------------------------------------------------------------
floreana <- dis_galapagos %>% 
  filter(islas_2 == "200152")

# santa_cruz ------------------------------------------------------------------
santa_cruz = dis_galapagos %>% 
  filter(islas == "2003")

# isabela ---------------------------------------------------------------------
isabela = dis_galapagos %>% 
  filter(islas == "2002")


nueva_galapagos_2 <- rbind(san_crsitobal[1:16,],
                           isabela[1:6,],
                           santa_cruz[1:9,],
                           isabela[7:11,],
                           floreana,
                           san_crsitobal[17:31,],
                           santa_cruz[10:61,]
) %>% mutate(semana = c(1:52,1:52)) %>% 
  mutate(periodo = ceiling(semana/4))

# --- CONTROLES
nueva_galapagos_2 %>% 
  group_by(islas,semana) %>% 
  summarise(n = n()) %>% 
  arrange(semana) %>% 
  pivot_wider(names_from = semana,
              values_from = n) %>% 
  arrange(islas) %>% 
  View()

nueva_galapagos_2 %>% 
  group_by(islas,periodo) %>% 
  summarise(n = n()) %>% 
  arrange(periodo) %>% 
  pivot_wider(names_from = periodo,
              values_from = n) %>% 
  View()

# --- EXPORTAR
export(nueva_galapagos_2,"nueva_galapagos.xlsx")

