# Analisis de variables #
# Francisca Apablaza #
# Participacion en el mercado laboral #

# En este segundo Scrip, tiene por objetivo realizar una #
# visualizacion de las variables ya recodificadas anteriormente. #

#Para esta primera entrega se espera que sean capaces de generar una tabla descriptiva
#que muestre las medidas de tendencia central de las variables utilizadas en la investigación,
#así como su contraparte de tablas de frecuencias en el caso de variables categóricas. 
#También se espera que sean capaces de elaborar dos gráficos descriptivos que permitan 
#visualizar la distribución de las principales variables de interés.

# 1. Cargar paquetes #
##
##
##
##

# Analisis de las variables  #
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
save(proc_base,file = "1. input/CASEN_recod.rdata")