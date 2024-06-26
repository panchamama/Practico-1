# Analisis de variables #
# Francisca Apablaza #
# Participacion en el mercado laboral #

# En este segundo Scrip, tiene por objetivo realizar una #
# visualizacion de las variables ya recodificadas anteriormente. #

# 0. Pasos previos # ----
rm(list = ls()) #limpiar area de trabajo
options(scipen = 999) #desactivar notacion cientifica

# 1. Cargar paquetes # ----
pacman::p_load(sjlabelled, dplyr, stargazer, sjmisc, summarytools, kableExtra, sjPlot, corrplot, sessioninfo, ggplot2)

# 2. Cargar base de datos # ----
load(url("https://github.com/panchamama/Practico-1/raw/main/1.%20input/casen_rec.rdata"))
# La base de datos recodificada, esta cargada desde internet.
names(casen_rec)
dim(casen_rec)

## 2.1 Recodificar base por edad ## ----
casen_rec <- casen_rec %>% dplyr:: filter(edad >= 18 & edad <= 65)

# 3. Descripcion de las variables # ----
## 3.1 Tablas descriptivas con descr, librería sjmiscsjmisc::descr ##
sjmisc::descr(casen_rec)
sjmisc::descr(casen_rec,
              show = c("label","range", "mean", "sd", "NA.prc", "n"))%>%
  kable(.,"markdown")

## 3.2 Tabla descriptiva con summarytools::dfSummarysummarytools::dfSummary ##
summarytools::dfSummary(casen_rec, plain.ascii = FALSE)
view(dfSummary(casen_rec, headings=FALSE)) # Visualizar mejor


# 4. Asociacion entre variables # ----

## a) Realizo trabajo formal ##
sjt.xtab(casen_rec$trabajo, casen_rec$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)

## b) Realizo trabajo de manera informal ##
sjt.xtab(casen_rec$trabajo_informal, casen_rec$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)

# c) Cuantas horas trabajo a la semana ##
sjt.xtab(casen_rec$horas_trabajo, casen_rec$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)

# d) Razon de la no busqueda de trabajo ##
sjt.xtab(casen_rec$busqueda_trabajo, casen_rec$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)

# e) Salario
sjt.xtab(casen_rec$sueldo, casen_rec$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)


# 5. Graficos de las variables # ----

## a) Ingreso de salario mensual ##

graph1 <- casen_rec %>% 
  filter(!is.na(sueldo)) %>% 
  ggplot(aes(x = sueldo, fill = sexo)) + 
  geom_bar() +
  xlab("Ingreso de sueldo mensual") +
  ylab("Cantidad") + 
  labs(fill="Sexo")+
  scale_fill_discrete(labels = c('Hombre','Mujer'))

graph1
ggsave(graph1, file="3. output/graph1.png")
