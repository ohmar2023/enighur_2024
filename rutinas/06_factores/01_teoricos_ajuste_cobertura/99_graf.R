aux <- muestra_primera_etapa %>% 
   group_by(pro) %>% 
   summarise(Ni_enlist = sum(Ni_enlist), 
             Ni_marco = sum(Ni), 
             diferencia = Ni_enlist - Ni_marco)

ylim <- c(0, 1.2*max(aux$diferencia))

barplot(aux$diferencia , border=F , names.arg = aux$pro , 
        las = 2 , 
        col = c("darkgreen", "bisque", "darkorange",  "darkorange") , 
        ylim = ylim , 
        main = "Diagrama de barras"
)   

# ----------------------------------------------------------------------------
# Cantidad de UPM por provincia que presentan una diferencia mayor a 100 viv ocup
# ----------------------------------------------------------------------------

aux_1 <- wk2 %>% 
  mutate(dif_Ni = Ni_enlist - Ni) #revisar los mayores a cero mas grandes

summary(aux_1$dif_Ni)  

aux_2 <- aux_1 %>% filter(dif_Ni > 100) %>% 
  group_by(pro) %>% 
  summarise(n_upm = n())

#aux_1 %>% filter(dif_Ni > 100) %>% View()

ylim <- c(0, 1.2*max(aux_2$n_upm))

barplot(aux_2$n_upm , border=F , names.arg = aux_2$pro , 
        las = 2 , 
        col = c("darkgreen", "bisque", "darkorange",  "darkorange") , 
        ylim = ylim , 
        main = "Diagrama de barras"
)   

# ----------------------------------------------------------------------------
# La UPM con mayor diferencia es 090150011801 (diferencia : 606 = 706 - 100)
# Periodo 09 semana 35
# Pluscode: 6792W2R4+R96
# ----------------------------------------------------------------------------

#Total enlistamiento
aux_upm_606 <- base_enlist_acum %>% filter(id_upm == "090150011801") 
dim(aux_upm_606)

#Viviendas ocupadas personas
  aux_upm_606 %>% 
  filter(grepl(c_ocup, pattern = "ocupada con|base ocupada|rechazo")) %>% 
  dim()

table(aux_upm_606$c_ocup)


# ----------------------------------------------------------------------------
# La UPM con mayor diferencia es 090150375501 (diferencia : 423 = 503 - 80)
# Periodo 08 semana 31
# Pluscode: 6792V25G+X92
# ----------------------------------------------------------------------------

#Total enlistamiento
aux_upm_423 <- base_enlist_acum %>% filter(id_upm == "090150375501") 
dim(aux_upm_423)

#Viviendas ocupadas personas
aux_upm_423 %>% 
  filter(grepl(c_ocup, pattern = "ocupada con|base ocupada|rechazo")) %>% 
  dim()

table(aux_upm_423$c_ocup)

# ----------------------------------------------------------------------------
# La UPM con mayor diferencia es 180150014402 (diferencia : 312 = 430 - 118)
# Periodo 02 semana 08
# Pluscode: 67C3Q978+4F2
# ----------------------------------------------------------------------------

#Total enlistamiento
aux_upm_312 <- base_enlist_acum %>% filter(id_upm == "180150014402") 
dim(aux_upm_312)

#Viviendas ocupadas personas
aux_upm_312 %>% 
  filter(grepl(c_ocup, pattern = "ocupada con|base ocupada|rechazo")) %>% 
  dim()

table(aux_upm_312$c_ocup)









