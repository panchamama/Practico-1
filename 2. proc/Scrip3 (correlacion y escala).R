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
casen_rec$horas_trabajo <- recode(casen_rec$horas_trabajo, "Inf promedio" = 0, "Promedio" = 0.5, "Sup promedio" = 1)
summary(casen_rec$horas_trabajo) #Confirmacion

frq(casen_rec$sueldo) # Recodificacion numerica
casen_rec$sueldo<- recode(casen_rec$sueldo, "Menos del salario minimo" = 0, "Salario promedio" = 0.5, "Salario sobre el promedio" = 1)
summary(casen_rec$sueldo) #Confirmacion

# 4. Matrices de Correlacion # ----
casen_correlacion <- casen_rec %>% select(trabajo_informal, # Trabajo informal
                                          horas_trabajo, # Horas trabajadas
                                          tipo_trabajo, # Su trabajo es de tipo
                                          contrato, # Tipo de contrato
                                          jornada_labora, #Jornada laboral
                                          horario_trabajo, #Tipo de horario
                                          sueldo) # Sueldo

## 4.1 Listwise delection ## ----
df <- casen_correlacion # Respaldar base de datos
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

# 5. Alfa de Cronbach # ----
# Asegúrate de tener el paquete psych instalado y cargado
if (!requireNamespace("psych", quietly = TRUE)) {
  install.packages("psych")
}
library(psych)

# Calcular el alpha de Cronbach con la opción check.keys=TRUE
alpha_result <- psych::alpha(casen_correlacion, check.keys=TRUE)

# Ver los resultados
print(alpha_result)

# Ver cuáles variables fueron invertidas
print(alpha_result$keys)

# 6. Construccion de la escala # ----
casen_rec <- casen_rec %>% 
  rowwise() %>% 
  mutate(participacion_mercado_laboral = sum(trabajo,trabajo_informal,horas_trabajo,tipo_trabajo,contrato,jornada_labora,horario_trabajo,sueldo))
summary(casen_rec$participacion_mercado_laboral)

frq(casen_rec$participacion_mercado_laboral)

ggplot(casen_rec, aes(x = participacion_mercado_laboral)) +
  geom_histogram(binwidth=0.6, colour="black", fill="yellow") +
  theme_bw() +
  xlab("Participacion en el mercado laboral") +
  ylab("Cantidad")

# 7. Guardar base de datos ## ----
casen_reg <- casen_rec %>% select(participacion_mercado_laboral,
                                  sexo,
                                  educ,
                                  nacionalidad,
                                  pueblo_originario)

save(casen_reg,file = "1. input/casen_reg.rdata")
