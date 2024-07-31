rm(list=ls())

library(rio)
library(tidyverse)
library(sampling)
library(stringr)
library(srvyr)
library(openxlsx)

# Muestra ENIGHUR-MACHALA 
mue_eni_mac <- tam_estrato

# Marco UPM-MACHALA 
marco_upm_01 <- marco_upm %>% 
  filter(domest=="401") %>% 
  select(-area)

# Marco Viviendas-MACHALA 
marco_viv_mach <- import("./insumos/marco_viv_mach.rds") %>% 
  rename(id_conglo=id_upm,id_edif=id_edificio)

# Partición Manzanas
part_man <- import("./insumos/particion_manzanas_li_60.rds") %>% 
  select(id_edif, grupo)

# Base personas-MACHALA
marco_per_mach <- import("./insumos/poblacion2022_machala.rds") %>% 
  rename_all(tolower) %>% 
  rename(id_vivienda=id_viv)

# construcción UPM a 12 dígitos
base <- left_join(marco_viv_mach,part_man,by="id_edif") %>% 
  mutate(grupo = ifelse(is.na(grupo), "01", grupo ),
         grupo =  str_pad(grupo, 2, "left", pad = "0"),
         id_upm = paste0(id_conglo,grupo))


# Selección de 200 muestras de UPM

muestra_upm <- vector("list",200)

for (i in 1:200) {

  muestra_upm[[i]] <- marco_upm_01 %>% 
    left_join(mue_eni_mac, by = "estrato") %>%  #aqui se están emparejando los tamaños por estrato nh 
    arrange(estrato, desc(Mi)) %>% 
    group_by(estrato) %>% 
    mutate(pik = inclusionprobabilities(Mi, unique(nh)),
           sel = UPrandomsystematic(pik, eps=1e-6)) %>% 
    ungroup() %>% 
    filter(sel==1) %>% 
    select(id_upm,estrato,Mi,nh,pik)
  
  print(i)
  
}


## Selección de viviendas 

muestra_viv <- vector("list", 200)

for (j in 1:length(muestra_viv)) {
  
  indice_upm = sort(unique(muestra_upm[[j]]$id_upm))
  
  n = 12
  
  for(i in 1:length(indice_upm)){
    print(i)
    viv = filter(base, id_upm==indice_upm[i])
    m_viv = sample_n(viv, n, replace = F)
    if (i==1){
      mv = m_viv
    } else {
      mv = rbind(mv, m_viv)
    }
    
    muestra_viv[[j]] <- mv %>% 
      left_join(muestra_upm[[j]],
                by = "id_upm") %>% 
      mutate(pi_usm = 12 / Mi,
             pi_viv = pik * pi_usm,
             fexp_viv = 1 / pi_viv) %>% 
      select(id_vivienda,estrato,fexp_viv,id_upm_12=id_upm)
  }
}


# Estimadores CENSO
indic_cen <- marco_per_mach %>% 
  summarise(do_med=mean(do_ciet19, na.rm = T),
            de_med=mean(de_ciet13, na.rm = T))


# Selección personas
muestra_per <- vector("list", 200)

for (i in 1:length(muestra_viv)) {

  indice_vivienda = sort(unique(muestra_viv[[i]]$id_vivienda))
  
  muestra_per[[i]] <- marco_per_mach %>% 
    filter(id_vivienda %in% indice_vivienda) %>% 
    left_join(muestra_viv[[i]],by="id_vivienda")  

  print(i)
  
}

indicadores01 <- vector("list", 200)

for (i in 1:length(indicadores01)) {
 
  dis_per <- muestra_per[[i]] %>% as_survey_design(ids = id_upm_12,
                                                   strata = estrato,
                                                   weights = fexp_viv,
                                                   nest = T)
  
  options(survey.lonely.psu = "adjust")
  
  indicadores01[[i]] <- dis_per %>% 
    summarise(do_ciet19 = survey_mean(do_ciet19, vartype = c("se"), na.rm = T),
              de_ciet13 = survey_mean(de_ciet13, vartype = c("se"), na.rm = T)) %>% 
    mutate(muestra = i)
  
  print(i)
   
}

indicadores01 <- do.call(rbind, indicadores01)

# Media de los estimadores de las 200 muestras
resumen <- indicadores01 %>% 
  summarise(med_do_est=mean(do_ciet19),
            med_do_err=mean(do_ciet19_se),
            med_de_est=mean(de_ciet13),
            med_de_err=mean(de_ciet13_se))

# Guardar Resultados

Resultados <- createWorkbook()

# Add some sheets to the workbook
addWorksheet(Resultados, "resumen_muestras")
addWorksheet(Resultados, "resumen_censo")

# Write the data to the sheets
writeData(Resultados, "resumen_muestras", x = resumen,row.names = F)
writeData(Resultados, "resumen_censo", x = indic_cen,row.names = F)

saveWorkbook(Resultados,overwrite =T,"./productos/indicadores.xlsx")



