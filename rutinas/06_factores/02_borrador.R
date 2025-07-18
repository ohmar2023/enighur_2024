

wk1 <- muestra_primera_etapa %>% 
  group_by(estrato) %>% 
  #mutate(mh_p = sum( !is.na(ki_p)) ) %>% 
  #filter(!is.na(ki_p)) %>% 
  
  # Probabilidad de inclusion de primera etapa teórica anual y el facto teórico
  mutate(ppe = pik,
         fexp = 1/ppe) %>% 
  # Calculamos el ajuste de cobertura de primera etapa y el factor ajustado del periodo
  mutate(a1h = mh / mh_p,
         fexp_pe_aju = fexp * a1h) %>% 
  # Se calula los estimadores de vivienda usando el factor ajustado de primera etapa
  mutate(Nh_est = Ni_enlist * fexp_pe_aju) %>% 
  group_by(estrato) %>% 
  mutate(Nh_est = round(sum(Nh_est), 0)) %>% 
  ungroup() 


wk2 <- wk1 %>% 
  select(- starts_with("Nh_est")) %>% 
  # Probabilidad de inclusion de segunda etapa teórica por UPM
  mutate(ki_p = ne + nr + re + ed,
         pse = ki_p/Ni_enlist) %>% # Ni DEL ENLISTAMIENTO ------------------------------------------cambiar
  # Factor de expansión teórico y factor de expansión ajustado por primera etapa
  mutate(d0 = 1 / (ppe * pse),
         d1 = a1h * d0) %>% 
  # Ajuste de elegibilidad desconocida
  group_by(estrato) %>%
  mutate(tki_h = sum(ki_p * d1),
         ted_h = sum(ed * d1)) %>%
  ungroup() %>%
  mutate(a2h = tki_h / (tki_h - ted_h),
         d2 = a2h * d1) %>%
  # Ajuste de no respondientes
  group_by(estrato) %>% 
  mutate(tre_h = sum(re * d2),
         tnr_h = sum(nr * d2)) %>% 
  ungroup() %>% 
  mutate(a3h = (tre_h + tnr_h)/tre_h,
         d3 = a3h * d2)


control <- wk2 %>% 
  group_by(pro, area, estrato, Nh) %>% 
  summarise(Nh_est = sum(Ni * fexp_pe_aju),
            tre_1 = sum(re * d1),
            tnr_1 = sum(nr * d1),
            tne_1 = sum(ne * d1),
            ted_1 = sum(ed * d1),
            tre_2 = sum(re * d2),
            tnr_2 = sum(nr * d2),
            tne_2 = sum(ne * d2),
            tre_3 = sum(re * d3)) %>% 
  ungroup() %>% 
  mutate(control1 = tre_1 + tnr_1 + tne_1 + ted_1,
         control2 = tre_2 + tnr_2 + tne_2,
         control3 = tre_3 + tne_2)

  
# plot(log(control$Nh_est), log(control$control1))
# abline(0, 1, col = "red")
# table(control$Nh_est - control$control1, useNA = "ifany")
# 
# 
# plot(log(control$control1), log(control$control2))
# abline(0, 1, col = "red")
# table(control$Nh_est - control$control2, useNA = "ifany")
# 
# plot(log(control$control2), log(control$control3))
# abline(0, 1, col = "red")
# table(control$Nh_est - control$control3, useNA = "ifany")

# summarys de los ajustes de cobertura
summary(wk2$a1h)
summary(wk2$a2h)
summary(wk2$a3h)

# expansion de personas con los diferentes factores
sum(wk2$totper * wk2$d0)
sum(wk2$totper * wk2$d1)
sum(wk2$totper * wk2$d2)
sum(wk2$totper * wk2$d3)

# Número de viviendas ocupadas marco
sum(control$Nh_est)

# Número de viviendas estimadas con el factor de diseño
sum(control$tre_1) +
  sum(control$tnr_1) +
  sum(control$tne_1) +
  sum(control$ted_1)

# Número de viviendas estimadas con el factor ajustado por elegibilidad desconocida
sum(control$tre_2) +
  sum(control$tnr_2) +
  sum(control$tne_2)

# Número de viviendas estimadas con el factor ajustado por no respuesta
sum(control$tre_3, na.rm = T) +
  sum(control$tne_2)


rio::export(wk2, "wk2_1er_resultado.xlsx")


