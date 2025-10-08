tot_pob <- function(dominio, si.area=F, si.sexo=F, gedad, anio){
  
  pp <- readRDS("intermedios/06_factores/pp_provincias_cantones_area_sexo_edad.rds")
  #dominio = "nac"; si.area = T; si.sexo = T; gedad = c(0, 14, 15, 99); anio = "202411"
  
  nc <- length(gedad)/2
  inicial <- unique(pp$edadi)
  final <- unique(pp$edadf)
  
  for(i in 1:nc){
    
    indexi <- sum(inicial <= gedad[1+2*(i-1)])
    indexf <- sum(final <= gedad[2*(i)])
    
    if(i==1){
      
      mge <- rep(i, indexf-indexi+1)
      
    }else{
      
      mge <- c(mge, rep(i, indexf-indexi+1))
      
    }
      
  }
  
  #print(glue("Los grupos de edad a considerar son {length(gedad)/2}, con conteo {table(mge)}"))
  
  b1 <- pp %>% 
    filter(edadi >= min(gedad) & edadf <= max(gedad)) %>% 
    cbind(gedad = mge)
  
  rm(nc, i, inicial, final, indexi, indexf, mge)
  
  dominio <- as.name(dominio)
  dominio <- enquo(dominio)
  
  anio <- as.name(anio)
  anio <- enquo(anio)
  
  gedad <- "gedad"
  gedad <- as.name(gedad)
  gedad <- enquo(gedad)
  
  area <- "area"
  area <- as.name(area)
  area <- enquo(area)
  
  sexo <- "sexo"
  sexo <- as.name(sexo)
  sexo <- enquo(sexo)
  
  if(si.area==T & si.sexo==T){
    
    b1 <- b1
    
  }else if(si.area==T & si.sexo==F){
    
    b1 <- mutate(b1, sexo="0")
    
  }else if(si.area==F & si.sexo==T){
    
    b1 <- mutate(b1, area="0")
    
  }else{
    
    b1 <- mutate(b1, area="0", sexo="0")
    
  }
  
  b2 <- do.call(select, list(b1, dominio, area, sexo, gedad, anio))
  b2 <- do.call(group_by, list(b2, dominio, area, sexo, gedad))
  b3 <- do.call(summarise_all, list(b2, sum))
  
  names(b3) <- c("dominio", "area", "sexo", "gedad", "t")
  return(ungroup(b3))
  
}


