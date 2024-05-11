# Preparacion Correlacion de variables  #
# Francisca Apablaza #
# Inequidad de genero en el mercado laboral #


## 1. Cargar paquetes ## ----
pacman::p_load(tidyverse,
               car, 
               haven,
               summarytools, 
               sjmisc,
               psych     
)

options(scipen = 999) 
rm(list = ls()) 

## 2. Cargar base de datos rocodificada ## ----
load("1. input/CASEN_recod.rdata")
dim(proc_base)
View(proc_base)
names(proc_base)

##2.1 Seleccion de variables ## ----
proc_base <- proc_base %>% select(sexo, # Sexo
                              jefe_h, # Jefatura del hogar
                              pareja, # Conformacion de parejas dentro del hogar
                              mp_vivienda, # Residencia en el hogar de padre o madre
                              trbj, # Trabajo
                              `trbj_no formal`, # No trabajo, pero realizo una actividad por salario
                              horas_trbj, # Horas trabajadas
                              Salario) # Sueldo
names(proc_base) #comprobacion
sjlabelled::get_label(proc_base) #atributo de la variable

## 2.2 Recodificacion Variable entre 0 y 1 ----
frq(proc_base$trbj) #Recodificar en 0 y 1
frq(proc_base$`trbj_no formal`) #Recodificar en 0 y 1
frq(proc_base$horas_trbj) #Recodificar en 0 y 1
frq(proc_base$jefe_h) #Recodificar en 0 y 1
frq(proc_base$pareja) #Recodificar en 0 y 1
frq(proc_base$mp_vivienda) #Recodificar en 0 y 1
frq(proc_base$Salario) # Recodificar en 0 y 1

# Donde 0 significa que hay mayor inequidad laboral y 1 que hay menor inequidad laboral

# Recodificacion variable trabajo # 
proc_base$trbj <- as.numeric(proc_base$trbj)
proc_base$trbj <- recode(proc_base$trbj, "1=1; 2=0")
summary(proc_base$trbj) #Cornfirmacion

# Recodificacion variable trbj no formal #
proc_base$`trbj_no formal` <- as.numeric(proc_base$`trbj_no formal`)
proc_base$`trbj_no formal` <- recode(proc_base$`trbj_no formal`, "1=0; 2=1")
summary(proc_base$`trbj_no formal`) #Cornfirmacion

# Recodificacion variable horas de trabajo #
proc_base$horas_trbj <- as.numeric(proc_base$horas_trbj)
proc_base$horas_trbj <- recode(proc_base$horas_trbj, "0=0; 1=0; 2=1; 3=0")
summary(proc_base$horas_trbj) #Cornfirmacion

# Recodificacion variable jefe de hogar #
proc_base$jefe_h <- as.numeric(proc_base$jefe_h)
proc_base$jefe_h <- recode(proc_base$jefe_h, "1=1; 2=0")
summary(proc_base$jefe_h) #Cornfirmacion

# Recodificacion variable pareja #
proc_base$pareja <- as.numeric(proc_base$pareja)
proc_base$pareja <- recode(proc_base$pareja, "1=1; 2=0")
summary(proc_base$pareja) #Cornfirmacion

# Recodificacion variable madre o padre en la vivienda #
proc_base$mp_vivienda <- as.numeric(proc_base$mp_vivienda)
proc_base$mp_vivienda <- recode(proc_base$mp_vivienda, "1=1; 2=0")
summary(proc_base$mp_vivienda) #Cornfirmacion

# Recodificacion variable salario #
proc_base$Salario <- as.numeric(proc_base$Salario)
proc_base$Salario <- recode(proc_base$Salario, "0:1=0; 2:32=1")
summary(proc_base$Salario) #Cornfirmacion

## 2.3 Recodificacion base de datos ## ----
CASEN_mujeres <- proc_base %>% dplyr::filter(sexo == "Mujer" )
CASEN_hombres <- proc_base %>% dplyr::filter(sexo == "Hombre")

view(dfSummary(proc_base, headings=FALSE, graph.col = FALSE)) 

## 3. Construccion del indice ## ----
## 3.1 Indice filtrado por hombres ##
CASEN_hombres <- CASEN_hombres %>% select(jefe_h,
                                pareja,
                                mp_vivienda,
                                trbj,
                                `trbj_no formal`,
                                horas_trbj,
                                Salario)  %>%
  mutate_all(~(as.numeric(.))) # Convertimos todas las variables a numéricas


CASEN_hombres = CASEN_hombres %>% 
  rowwise() %>%
  mutate(participacion_laboral= mean(c(trbj,`trbj_no formal`,horas_trbj)),
         Composicion_del_hogar= mean(c(jefe_h, pareja, mp_vivienda)),
         Salario= mean(c(Salario))) %>% 
  ungroup()

CASEN_hombres = CASEN_hombres %>% 
  rowwise() %>%
  mutate(inequidad_laboral = mean(c(participacion_laboral, Composicion_del_hogar, Salario), na.rm = T)) %>% 
  ungroup() 

summary(CASEN_hombres$inequidad_laboral) # Resumen

## 3.2 Indice filtrado por mujeres ## 
CASEN_mujeres <- CASEN_mujeres %>% select(jefe_h,
                                      pareja,
                                      mp_vivienda,
                                      trbj,
                                      `trbj_no formal`,
                                      horas_trbj,
                                      Salario)  %>%
  mutate_all(~(as.numeric(.))) # Convertimos todas las variables a numéricas

CASEN_mujeres = CASEN_mujeres %>% 
  rowwise() %>%
  mutate(participacion_laboral= mean(c(trbj,`trbj_no formal`,horas_trbj)),
         Composicion_del_hogar= mean(c(jefe_h, pareja, mp_vivienda)),
         Salario= mean(c(Salario))) %>% 
  ungroup()

CASEN_mujeres = CASEN_mujeres %>% 
  rowwise() %>%
  mutate(inequidad_laboral = mean(c(participacion_laboral, Composicion_del_hogar, Salario), na.rm = T)) %>% 
  ungroup() 

summary(CASEN_mujeres$inequidad_laboral) # Resumen

## Guardar base de datos ##
save(proc_base,file = "1. input/CASEN_recodd.rdata")