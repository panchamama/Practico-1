# Correlacion y construccion de escala #
# Francisca Apablaza #
# Participacion en el mercado laboral #

# 1. Instalacion de paquetes # ----
rm(list = ls()) #limpiar area de trabajo

pacman::p_load(dplyr, # Manipulacion datos
               sjmisc, # Descriptivos
               sjPlot, # Tablas
               sjlabelled, #etiquetas
               kableExtra, #Tablas
               GGally, # Correlaciones
               corrplot) # Correlaciones

options(scipen = 999) # para desactivar notacion cientifica

# 2. Cargar base de datos rocodificada # ----
load(url("https://github.com/panchamama/Practico-1/raw/main/1.%20input/casen_rec.rdata"))
dim(casen_rec)
casen_rec <- casen_rec %>% dplyr:: filter(edad >= 18 & edad <= 65) # Recodificacion por edad

sjmisc::descr(casen_rec,
              show = c("label","range", "mean", "sd", "NA.prc", "n")) %>%
  kable(.,"markdown")

# 3. Seleccion de variables # ----
casen <- casen_rec %>% select(sexo, # Sexo
                              educ, # Nivel educacional mas alto al cual asistio
                              trabajo, # Trabajo
                              trabajo_informal, # Trabajo informal
                              horas_trabajo, # Horas trabajadas
                              tipo_trabajo, # Su trabajo es de tipo
                              contrato, # Tipo de contrato
                              jornada_labora, #Jornada laboral
                              horario_trabajo, #Tipo de horario
                              sueldo, # Sueldo
                              nacionalidad, # Pais de nacionalidad
                              pueblo_originario) # Pertenece a algun pueblo indigena

# En este Scrip no trabajare con todas las variables seleccionadas, si no solo con
# las cuales pueda construir la escala, sin embargo seleccione otras para almomento de
# guardar la base, guardar tambien esas variables, ya que las usare en el siguiente scrip.

## 3.1 Recodificacion de variables ## ----
frq(casen$sexo) # No es necesario recodificar
frq(casen$educ) # No es necesario recodificar

frq(casen$trabajo) # Recodificacion numerica
casen$trabajo <- recode(casen$trabajo, "No trabaja" = 0, "Trabaja" = 1)
summary(casen$trabajo) #Confirmacion

frq(casen$trabajo_informal) # Recodificacion numerica
casen$trabajo_informal <- recode(casen$trabajo_informal, "No trabaja" = 0, "Trabaja" = 1)
summary(casen$trabajo_informal) #Confirmacion

frq(casen$horas_trabajo) # Recodificacion numerica
casen$horas_trabajo<- recode(casen$horas_trabajo, "Inf promedio" = 0, "Promedio" = 0.5, "Sup promedio" = 1)
summary(casen$horas_trabajo) #Confirmacion

frq(casen$tipo_trabajo) # No es necesario recodificar
frq(casen$contrato) # No es necesario recodificar
frq(casen$jornada_labora) # No es necesario recodificar
frq(casen$horario_trabajo) # No es necesario recodificar

frq(casen$sueldo) # Recodificacion numerica
casen$sueldo<- recode(casen$sueldo, "Menos del salario minimo" = 0, "Salario promedio" = 0.5, "Salario sobre el promedio" = 1)
summary(casen$sueldo) #Confirmacion

frq(casen2$nacionalidad) # No es necesario recodificar
frq(casen2$pueblo_originario) # No es necesario recodificar

# 4. Matrices de Correlacion # ----
casen_correlacion <- casen %>% select(trabajo, # Trabajo
                                      trabajo_informal, # Trabajo informal
                                      horas_trabajo, # Horas trabajadas
                                      tipo_trabajo, # Su trabajo es de tipo
                                      contrato, # Tipo de contrato
                                      jornada_labora, #Jornada laboral
                                      horario_trabajo, #Tipo de horario
                                      sueldo) # Sueldo
## 4.1 Casos NA ## ----
mean(casen_correlacion$trabajo); mean(casen_correlacion$trabajo_informal); 
mean(casen_correlacion$horas_trabajo); mean(casen_correlacion$tipo_trabajo);
mean(casen_correlacion$contrato); mean(casen_correlacion$jornada_labora);
mean(casen_correlacion$horario_trabajo); mean(casen_correlacion$sueldo)
  
mean(casen_correlacion$trabajo, na.rm = TRUE); mean(casen_correlacion$trabajo_informal, na.rm = TRUE);
mean(casen_correlacion$horas_trabajo, na.rm = TRUE); mean(casen_correlacion$tipo_trabajo, na.rm = TRUE);
mean(casen_correlacion$contrato, na.rm = TRUE); mean(casen_correlacion$jornada_labora, na.rm = TRUE);
mean(casen_correlacion$horario_trabajo, na.rm = TRUE); mean(casen_correlacion$sueldo, na.rm = TRUE);

## 4.2 Correlacion ## ----
cor(casen_correlacion, use = "pairwise.complete.obs") # Correlacion

M <- cor(casen_correlacion, use = "complete.obs")
M

sjPlot::tab_corr(casen_correlacion, 
                 triangle = "lower")

sjPlot::tab_corr(proc_elsoc_original, 
                 na.deletion = "pairwise", # espeficicamos tratamiento NA
                 triangle = "lower")

corrplot.mixed(M)
 
# 5. Construccion de la escala #  ----


## Guardar base de datos ## ----
casan_reg <
save(proc_base,file = "1. input/CASEN_recodd.rdata")
 