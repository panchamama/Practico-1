# Correlacion y construccion de escala #
# Francisca Apablaza #
# Participacion en el mercado laboral #

# 1. Instalacion de paquetes # ----
pacman::p_load(dplyr, # Manipulacion datos
               sjmisc, # Descriptivos
               sjPlot, # Tablas
               sjlabelled, #etiquetas
               kableExtra, #Tablas
               GGally, # Correlaciones
               corrplot) # Correlaciones

options(scipen = 999) # para desactivar notacion cientifica

## 2. Cargar base de datos rocodificada ## ----
load(url("https://github.com/panchamama/Practico-1/raw/main/1.%20input/casen_rec.rdata"))
dim(casen_rec)
View(casen_rec)
names(casen_rec)

# 3. Seleccion de variables # ----
casen <- proc_base %>% select(edad, # Edad
                              sexo, # Sexo
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

# Estadisticos descriptivos 
sjmisc::descr(casen2,
              show = c("label","range", "mean", "sd", "NA.prc", "n")) %>%
  kable(.,"markdown")

# 3.1 Casos perdidos #
# a) Analisis con casos completos: listwise

# 3.2 Recodificacion de variables # ----
frq(casen2$edad) # No es necesario recodificar
frq(casen2$sexo) # No es necesario recodificar
frq(casen2$educ) # No es necesario recodificar

frq(casen2$trabajo) # Recodificacion numerica

frq(casen2$trabajo_informal) # Recodificacion numerica

frq(casen2$horas_trabajo) # Recodificacion numerica

frq(casen2$tipo_trabajo) # No es necesario recodificar
frq(casen2$contrato) # No es necesario recodificar
frq(casen2$jornada_labora) # No es necesario recodificar
frq(casen2$horario_trabajo) # No es necesario recodificar

frq(casen2$sueldo) # Recodificacion numerica

frq(casen2$nacionalidad) # No es necesario recodificar
frq(casen2$pueblo_originario) # No es necesario recodificar



#Construccion de Escala #


## Guardar base de datos ##
save(proc_base,file = "1. input/CASEN_recodd.rdata")



