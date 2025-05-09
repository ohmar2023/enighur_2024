
library(readr)

base_2 <- read_delim("insumos/03_enlistamiento/2024_12_13/Base_muestral___20241213_102759.csv", 
                                            delim = ";", escape_double = FALSE, trim_ws = TRUE) 
  
  
 base <-  base %>% clean_names() %>% mutate(id_upm = upm, 
                                            tot_hbt = as.numeric(tot_hbt),
                                            id_upm = str_pad(id_upm,12,"left","0"),
                                            man_nloc = ifelse(!is.na(man), man, n_loc),
                                            id_edif = paste0(pro, can, par, zon, sec, man_nloc, n_umce),
                                            id_viv = paste0(pro, can, par, zon, sec, man_nloc, n_umce, n_viv))


p <- base_2 %>% filter(upm=="030150005201") 
table(p$piso_n) 
