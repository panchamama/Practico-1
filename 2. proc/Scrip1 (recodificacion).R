# Preparacion Base de datos CASEN #
# Francisca Apablaza #
# Participacion en el mercado laboral #

# El presente Scrip es la preparacion de la base de datos CASEN #
# realizando la seleccion de variable y recodificarlas, de esta #
# forma trabajar solo con o necesario para el trabajo. :) #

# 1. Instalacion de paquetes # ----
install.packages("pacman")
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven, summarytools, kableExtra, sjPlot, corrplot, sessioninfo, ggplot2)

# 2. Pasos previos # ----
rm(list = ls()) #limpiar area de trabajo
options(scipen = 999) #desactivar notacion cientifica

# 3. Base de Datos # ----
CASEN <- read_sav("C:/Users/Asus/Desktop/2024 Primer Semestre/Opt. Rstudios/Practico 1/CASEN.sav")
dim(CASEN)

# 4. Seleccion de Variables # ----
proc_base <- CASEN %>% select(edad, # Edad
                              sexo, # Sexo
                              e6a_no_asiste, # Nivel educacional mas alto al cual asistio
                              o1, # Trabajo
                              o2, # Trabajo informal
                              o7, # Razon por la cual no busco trabajo
                              o10, # Horas trabajadas
                              o12, # Su trabajo es de tipo
                              o19, # Tipo de contrato
                              o20, #Jornada laboral
                              o21, #Tipo de horario
                              y1, # Sueldo
                              r1a, # Pais de nacionalidad
                              r3) # Pertenece a algun pueblo indigena
                              
names(proc_base) #comprobacion
sjlabelled::get_label(proc_base) #atributo de la variable

## 4.1 Renombramiento de las variables ## ----
proc_base <- proc_base %>% rename("educ"=e6a_no_asiste, # Nivel educacional
                                  "trabajo"=o1, # Tiene trabajo formal
                                  "trabajo_informal"=o2, # Tiene trabajo informal
                                  "busqueda_trabajo"=o7, # Porque no busco trabajo
                                  "horas_trabajo"=o10, # Horas trabajdas
                                  "tipo_trabajo"=o12, # Que tipo de permanencia tiene en al trabjo
                                  "contrato"=o19, # Tipo de contrato de trabajo
                                  "jornada_labora"=o20, # Jornada laboral
                                  "horario_trabajo"=o21, # Horario laboral
                                  "sueldo"=y1, # Sueldo
                                  "nacionalidad"=r1a, # Pais de nacionalidad
                                  "pueblo_originario"=r3) # Pertenece a algun pueblo originario
names(proc_base)

## 4.2 Re-etiquetado de las variables ## ----
proc_base$sexo <- set_label(x = proc_base$sexo, label = "Sexo")
get_label(proc_base$sexo)

proc_base$edad <- set_label(x = proc_base$edad, label = "Edad")
get_label(proc_base$edad)

proc_base$educ <- set_label(x = proc_base$educ, label = "Nivel educacional")
get_label(proc_base$educ)

proc_base$trabajo <- set_label(x = proc_base$trabajo, label = "Trabajo formal")
get_label(proc_base$trabajo)

proc_base$trabajo_informal <- set_label(x = proc_base$trabajo_informal, label = "Trabajo informal")
get_label(proc_base$trabajo_informal)

proc_base$busqueda_trabajo <- set_label(x = proc_base$busqueda_trabajo, label = "Porque no busco trabajo")
get_label(proc_base$busqueda_trabajo)

proc_base$horas_trabajo <- set_label(x = proc_base$horas_trabajo, label = "Horas trabajadas")
get_label(proc_base$horas_trabajo)

proc_base$tipo_trabajo <- set_label(x = proc_base$tipo_trabajo, label = "Tipo de permanencia en el trabajo")
get_label(proc_base$tipo_trabajo)

proc_base$contrato <- set_label(x = proc_base$contrato, label = "Tipo de contrato de trabajo")
get_label(proc_base$contrato)

proc_base$jornada_labora <- set_label(x = proc_base$jornada_labora, label = "Jornada laboral")
get_label(proc_base$jornada_labora)

proc_base$horario_trabajo <- set_label(x = proc_base$horario_trabajo, label = "Horario de trabajo")
get_label(proc_base$horario_trabajo)

proc_base$sueldo <- set_label(x = proc_base$sueldo, label = "Sueldo mensual")
get_label(proc_base$sueldo)

proc_base$nacionalidad <- set_label(x = proc_base$nacionalidad, label = "Pais de nacionalidad")
get_label(proc_base$nacionalidad)

proc_base$pueblo_originario <- set_label(x = proc_base$pueblo_originario, label = "Pertenece a algun pueblo originario")
get_label(proc_base$pueblo_originario)

## 4.3 Recodificacion casos perdidos NA ## ----

frq(proc_base$edad) # No es necesario recodificar
frq(proc_base$sexo) # No es necesario recodificar
frq(proc_base$educ) # No es necesario recodificar
frq(proc_base$trabajo) # No es necesario recodificar
frq(proc_base$trabajo_informal) # No es necesario recodificar
frq(proc_base$busqueda_trabajo) # No es necesario recodificar

frq(proc_base$horas_trabajo) # Si es necesario recodificar
proc_base$horas_trabajo <- recode(proc_base$horas_trabajo, "c(-88)=NA")
proc_base$horas_trabajo <- as.numeric(factor(proc_base$horas_trabajo))

frq(proc_base$tipo_trabajo) # Si es necesario recodificar
proc_base$tipo_trabajo <- recode(proc_base$tipo_trabajo, "c(-88, -99)=NA")
proc_base$tipo_trabajo <- as.numeric(factor(proc_base$tipo_trabajo))

frq(proc_base$contrato) # Si es necesario recodificar
proc_base$contrato <- recode(proc_base$contrato, "c(-88)=NA")
proc_base$contrato <- as.numeric(factor(proc_base$contrato))

frq(proc_base$jornada_labora) # Si es necesario recodificar
proc_base$jornada_labora <- recode(proc_base$jornada_labora, "c(-88, -99)=NA")
proc_base$jornada_labora <- as.numeric(factor(proc_base$jornada_labora))

frq(proc_base$horario_trabajo) # Si es necesario recodificar
proc_base$horario_trabajo <- recode(proc_base$horario_trabajo, "c(-88, -99)=NA")
proc_base$horario_trabajo <- as.numeric(factor(proc_base$horario_trabajo))

frq(proc_base$sueldo) # Si es necesario recodificar -88
proc_base$sueldo <- recode(proc_base$sueldo, "c(-88)=NA")
proc_base$sueldo <- as.numeric(factor(proc_base$sueldo))

frq(proc_base$nacionalidad) # No es necesario recodificar
frq(proc_base$pueblo_originario) # No es necesario recodificar

# 5. Recodificacion de las variables # ----

## Sexo ##
frq(proc_base$sexo) #Descriptivo
proc_base$sexo <- recode(proc_base$sexo, "1=1; 2=0") 
proc_base$sexo <- factor(proc_base$sexo, levels = c("0", "1"), labels = c("Mujer", "Hombre")) #Recodificar
summary(proc_base$sexo) #Cornfirmacion

## Nivel educacion ## 
frq(proc_base$educ) #Descriptivo
proc_base$educ <- recode(proc_base$educ, "1=0; 2:7=1; 8:11=2; 12:13=3; 14:15=4") 
proc_base$educ <- factor(proc_base$educ, levels = c("0", "1", "2", "3", "4"), labels = c("Sin estudios", "Educacion basica", "Educacion media", "Educacion superior", "Postgrados")) #Recodificar
summary(proc_base$educ) #Confirmacion

## Trabajo formal ## 
frq(proc_base$trabajo) #Descriptivo
proc_base$trabajo <- recode(proc_base$trabajo, "1=1; 2=0") 
proc_base$trabajo <- factor(proc_base$trabajo, levels = c("0", "1"), labels = c("No trabaja", "Trabaja")) #Recodificar
summary(proc_base$trabajo) #Confirmacion

## Trabajo informal ## 
frq(proc_base$trabajo_informal) #Descriptivo
proc_base$trabajo_informal <- recode(proc_base$trabajo_informal, "1=1; 2=0") 
proc_base$trabajo_informal <- factor(proc_base$trabajo_informal, levels = c("0", "1"), labels = c("No trabaja", "Trabaja")) #Recodificar
summary(proc_base$trabajo_informal) #Confirmacion

## Porque no busco trabajo ## 
frq(proc_base$busqueda_trabajo) #Descriptivo
proc_base$busqueda_trabajo <- recode(proc_base$busqueda_trabajo, "c(1,2,6,7,8,9,11,12,13,14,15,16,17,18,19)=0; c(3,4,5,10)=1") 
proc_base$busqueda_trabajo <- factor(proc_base$busqueda_trabajo, levels = c("0", "1"), labels = c("Otro motivo", "Razones de cuidado")) #Recodificar
summary(proc_base$busqueda_trabajo) #Confirmacion

## Horas trabajadas semanalmente ##
frq(proc_base$horas_trabajo) #Descriptivo
proc_base$horas_trabajo <- recode(proc_base$horas_trabajo, "1:20=0; 21:40=0.5; 41:92=1") 
proc_base$horas_trabajo <- factor(proc_base$horas_trabajo, levels = c("0", "0.5", "1"), labels = c("Inf promedio", "Promedio", "Sup promedio")) #Recodificar
summary(proc_base$horas_trabajo) #Confirmacion

## Tipo de permanencia en el trabajo ##
frq(proc_base$tipo_trabajo) #Descriptivo
proc_base$tipo_trabajo <- recode(proc_base$tipo_trabajo, "1=1; c(2,5)=0.5; c(3,4)=0") 
summary(proc_base$tipo_trabajo) #Confirmacion

## Tipo de contrato ##
frq(proc_base$contrato) #Descriptivo
proc_base$contrato <- recode(proc_base$contrato, "1=1; 2=0.5; 3=0") 
summary(proc_base$contrato) #Confirmacion

## Jornada laboral ##
frq(proc_base$jornada_labora) #Descriptivo
proc_base$jornada_labora <- recode(proc_base$jornada_labora, "1=1; 2:3=0.5; 4=0") 
summary(proc_base$jornada_labora) #Confirmacion

## Horario de trabajo ##
frq(proc_base$horario_trabajo) #Descriptivo
proc_base$horario_trabajo <- recode(proc_base$horario_trabajo, "1=1; 2=0.5; 3=0") 
summary(proc_base$horario_trabajo) #Confirmacion

## Salario mensual ## 
frq(proc_base$sueldo) #Descriptivo
proc_base$sueldo <- recode(proc_base$sueldo, "1:500=0; 501:899=0.5; 900:1185=1") 
proc_base$sueldo <- factor(proc_base$sueldo, levels = c("0", "0.5", "1"), labels = c("Menos del salario minimo", "Salario promedio", "Salario sobre el promedio")) #Recodificar
summary(proc_base$sueldo) #Confirmacion

## Nacionalidad ##
frq(proc_base$nacionalidad) #Descriptivo
proc_base$nacionalidad <- recode(proc_base$nacionalidad, "1:2=0 ; 3=1") 
proc_base$nacionalidad <- factor(proc_base$nacionalidad, levels = c("0", "1"), labels = c("Chilena", "Extranjera")) #Recodificar
summary(proc_base$nacionalidad) #Confirmacion

## Pueblo originario ##
frq(proc_base$pueblo_originario) #Descriptivo
proc_base$pueblo_originario <- recode(proc_base$pueblo_originario, "1:10=1; 11=0") 
proc_base$pueblo_originario <- factor(proc_base$pueblo_originario, levels = c("0", "1"), labels = c("No pertenece a pueblo originario", "Pertenece a pueblo originario")) #Recodificar
summary(proc_base$pueblo_originario) #Confirmacion

# 6. Guardar base de datos recodificada # ----
proc_base <-as.data.frame(proc_base)
stargazer(proc_base, type="text") # Revision de variables con tabla descriptiva

save(proc_base,file = "1. input/casen_rec.rdata") # Guardar base de datos recodificada