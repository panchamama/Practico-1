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
casen_rec <- casen_rec %>% dplyr:: filter(edad >= 18 & edad <= 65) # Recodificacion por edad
dim(casen_rec)

sjmisc::descr(casen_rec,
              show = c("label","range", "mean", "sd", "NA.prc", "n")) %>%
  kable(.,"markdown")

# 3. Recodificacion de variables, solo las que son necesarias # ----
frq(casen_rec$trabajo) # Recodificacion numerica
casen_rec$trabajo <- recode(casen_rec$trabajo, "No trabaja" = 0, "Trabaja" = 1)
summary(casen_rec$trabajo) #Confirmacion

frq(casen_rec$trabajo_informal) # Recodificacion numerica
casen_rec$trabajo_informal <- recode(casen_rec$trabajo_informal, "No trabaja" = 0, "Trabaja" = 1)
summary(casen_rec$trabajo_informal) #Confirmacion

frq(casen_rec$horas_trabajo) # Recodificacion numerica
casen_rec$horas_trabajo<- recode(casen_rec$horas_trabajo, "Inf promedio" = 0, "Promedio" = 0.5, "Sup promedio" = 1)
summary(casen_rec$horas_trabajo) #Confirmacion

frq(casen_rec$sueldo) # Recodificacion numerica
casen_rec$sueldo<- recode(casen_rec$sueldo, "Menos del salario minimo" = 0, "Salario promedio" = 0.5, "Salario sobre el promedio" = 1)
summary(casen_rec$sueldo) #Confirmacion

# 4. Matrices de Correlacion # ----
casen_correlacion <- casen_rec %>% select(trabajo, # Trabajo
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
M <- cor(casen_correlacion, use = "complete.obs")
M

sjPlot::tab_corr(casen_correlacion, 
                 na.deletion = "pairwise", # espeficicamos tratamiento NA
                 triangle = "lower")

corrplot.mixed(M)
cor(casen_correlacion, use = "pairwise.complete.obs") # Correlacion

# 4.a Listwise delection # ----
casen_original <- casen_correlacion # Respaldar base de datos
dim(casen_correlacion)

sum(is.na(casen_correlacion)) # Contamos los NA.s

colSums(is.na(casen_correlacion)) # Vemos donde estas los NA.s

casen_correlacion <- na.omit(casen_correlacion) # Eliminamos los NA.s
dim(casen_correlacion)

M <- cor(casen_correlacion, use = "complete.obs") # Matriz de correlacion
M

sjPlot::tab_corr(M, # Visualizacion
                 triangle = "lower")

corrplot.mixed(M)

# 5. Construccion de la escala #  ----

na.omit(casen_correlacion)
cor(casen_correlacion)

# Alfa de Cronbach #
psych::alpha(casen_correlacion)

casen_correlacion <- casen_correlacion %>% 
  rowwise() %>% 
  mutate(participacion_mercado_laboral = sum(trabajo,trabajo_informal,horas_trabajo,tipo_trabajo,contrato,jornada_labora,horario_trabajo,sueldo))
summary(casen_correlacion$participacion_mercado_laboral)

## Guardar base de datos ## ----
casan_reg <
save(proc_base,file = "1. input/CASEN_recodd.rdata")
 