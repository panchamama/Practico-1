# Preparacion Base de datos CASEN #
# Francisca Apablaza #
# Inequidad de genero en el mercado laboral #

# 1. Instalacion de paquetes # ----
install.packages("pacman")
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven, summarytools, kableExtra, sjPlot, corrplot, sessioninfo, ggplot2)

# 2. Pasos previos # ----
rm(list = ls()) #limpiar area de trabajo
options(scipen = 999) #desactivar notacion cientifica

# 3. Base de Datos # ----
CASEN <- read_sav("C:/Users/Asus/Documents/GitHub/Practico-1/1. input/CASEN.sav")
dim(CASEN)
View(CASEN)
names(CASEN)

# 4. Seleccion de Variables # ----
proc_base <- CASEN %>% select(edad, # Edad
                              sexo, # Sexo
                              pco1_a, # Jefatura del hogar
                              h5_cp, # Conformacion de parejas dentro del hogar
                              h5_10, # Residencia en el hogar de padre o madre
                              o1, # Trabajo
                              o2, # No trabajo, pero realizo una actividad por salario
                              o7, # Razon por la cual no busco trabajo
                              o10, # Horas trabajadas
                              y1) # Sueldo
names(proc_base) #comprobacion
sjlabelled::get_label(proc_base) #atributo de la variable

# 4.1 Filtrar base de datos por edad #
proc_base <- filter(proc_base, edad >= 25 & edad <= 65)

# 5. Procesamiento de variables # ----

## 5.1 Participacion en el mercado laboral ## ----
frq(proc_base$o1) #Descriptivo
proc_base$o1 <- factor(proc_base$o1, levels = c("1", "2"), labels = c("Sí", "No")) #Recodificar
summary(proc_base$o1) #Cornfirmacion

frq(proc_base$o2) #Descriptivo
proc_base$o2 <- factor(proc_base$o2, levels = c("1", "2"), labels = c("Sí", "No")) #Recodificar
summary(proc_base$o2) #Confirmacion

frq(proc_base$o10) #Descriptivo
proc_base$o10 <- recode(proc_base$o10, "c(-88)=NA")
proc_base$o10 <- recode(proc_base$o10, "0=0; 1:30=1; 31:45=2; 45:85=3")
proc_base$o10 <- set_labels(proc_base$o10,
                            labels=c( "No_trbj"=0,
                                      "Trbj_me8"=1,
                                      "Trbj_8"=2,
                                      "Trbj_ma8"=3))
summary(proc_base$o10) #Confirmacion

# Etiquetado
proc_base <- proc_base %>% rename("trbj"=o1, # Trabajo
                                  "trbj_no formal"=o2, # No trabajo, pero realizo una actividad por salario
                                  "horas_trbj"=o10) # Horas de trabajo

## 5.2 Rol de la familia ## ----
frq(proc_base$pco1_a) #Descriptivo
proc_base$pco1_a <- factor(proc_base$pco1_a, levels = c("0", "1"), labels = c("Sí", "No")) #Recodificar
summary(proc_base$pco1_a) #Cornfirmacion

frq(proc_base$h5_cp) #Descriptivo
proc_base$h5_cp <- factor(proc_base$h5_cp, levels = c("1", "2"), labels = c("Sí", "No")) #Recodificar
summary(proc_base$h5_cp) #Cornfirmacion

frq(proc_base$h5_10) #Descriptivo
proc_base$h5_10 <- factor(proc_base$h5_10, levels = c("1", "2"), labels = c("Sí", "No")) #Recodificar
summary(proc_base$h5_10) #Cornfirmacion

frq(proc_base$o7) #Descriptivo
proc_base$o7 <- recode(proc_base$o7, "3=1; 4=2; 5=3; 10=4; 1:2=0; 6:9=0; 11:19=0") #Recodificacion
proc_base$o7 <- set_labels(proc_base$o7, 
                           labels = c("Tiene que cuidar a niños o niñas"=1,
                                      "Tiene que cuidar a algún adulto mayor"=2,
                                      "Tiene que cuidar a otro familiar"=3,
                                      "Quehaceres del hogar"=4,
                                      "Otra razon"=0))
summary(proc_base$o7) #Cornfirmacion

#Etiquetado
proc_base <- proc_base %>% rename("jefe_h"=pco1_a, # Jefe de hogar
                                  "pareja"=h5_cp, # Pareja dentro del hogar
                                  "mp_vivienda"=h5_10, # vive el madre o el padre en la vivienda
                                  "busqueda_trbj"=o7) #Porque no busco trabajo 

## 5.3 Brecha salarial ## ----
frq(proc_base$y1) #Descriptivo
proc_base$y1 <- recode(proc_base$y1, "c(-88)=NA")#Recodificacion
proc_base$y1 <- recode(proc_base$y1, "209000:326200=1; 326920:1000000=2; 1000050:25000000=3; 0:208000=0")
proc_base$y1 <- set_labels(proc_base$y1,
                           labels=c( "Sueldo inf"=1,
                                     "Sueldo pro"=2,
                                     "Sueldo sup"=3,
                                     "Sin sueldo"=0))
summary(proc_base$y1) #Cornfirmacion

#Etiquetado
proc_base <- proc_base %>% rename("Salario"=y1) #Salario 

## 5.4 Sexo ## ----
frq(proc_base$sexo) #Descriptivo
proc_base$sexo <- car::recode(proc_base$sexo, "1=0;2=1")
proc_base$sexo <- factor(proc_base$sexo,
                         labels=c( "Hombre",
                                   "Mujer"),
                         levels=c(0,1))
get_label(proc_base$sexo)
frq(proc_base$sexo)
summary(proc_base$sexo) #Cornfirmacion

# 6. Analisis de las variables  # ----

## 6.1 Revicion descriptiva de los datos ## ----
names(proc_base) # Muestra los nombres de las variables de la base de datos
dim(proc_base) # Dimensiones
sjmisc::descr(proc_base,
              show = c("label","range", "mean", "sd", "NA.prc", "n"))%>%
              kable(.,"markdown")
summarytools::dfSummary(proc_base, plain.ascii = FALSE)
view(dfSummary(proc_base, headings=FALSE))

## 6.2 Visualizacion de variables ## ----

### Participacion en el mercado laboral ### ----
# a) Realizo trabajo, sin contar trabajo en el hogar 
sjt.xtab(proc_base$trbj, proc_base$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)

# b) Realizo trabajo de manera informal
sjt.xtab(proc_base$`trbj_no formal`, proc_base$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)

# c) Cuantas horas trabajo a la semana
sjt.xtab(proc_base$horas_trbj, proc_base$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)

### Rol de la familia ### ----
# d) Jefe de hogar
sjt.xtab(proc_base$jefe_h, proc_base$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)

# e) Conformacion de parejas dentro del hogar
sjt.xtab(proc_base$pareja, proc_base$trbj,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)

# f) El padre o la madre viven en la vivienda
sjt.xtab(proc_base$mp_vivienda, proc_base$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)

# g) Razon de la no busqueda de trabjao
sjt.xtab(proc_base$busqueda_trbj, proc_base$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)

### Brecha salarial ### ----
# h) Salario
# Crear el gráfico usando ggplot2
sjt.xtab(proc_base$Salario, proc_base$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)

graph8 <- proc_base %>% ggplot(aes(x = Salario, fill = sexo)) + 
  geom_bar() +
  xlab("Salario") +
  ylab("Cantidad") + 
  labs(fill="Sexo")+
  scale_fill_discrete(labels = c('Hombre','Mujer'))

graph8
ggsave(graph8, file="3. output/graph8.png")

# 7. Guardar # ----
save(proc_base,file = "C:/Users/Asus/Documents/GitHub/Practico-1/2. proc")