# Regresion lineal #
# Francisca Apablaza #
# Participacion en el mercado laboral #

# 1. Instalacion de paquetes # ----
rm(list = ls()) #limpiar area de trabajo

pacman::p_load(dplyr,
               car, 
               sjmisc, 
               sjPlot, 
               sjlabelled, 
               stargazer, 
               kableExtra, 
               corrplot, 
               texreg, 
               ggplot2, 
               ggpubr,
               summarytools, # Tablas
               sessioninfo, # Información de la sesión de trabajo
               fastDummies, # Generar regresión
               ggeffects) # Gráfico de valores predichos

# 2. Cargar la base de datos ----
load(url("https://github.com/panchamama/Practico-1/raw/main/1.%20input/casen_reg.rdata"))
dim(casen_reg)

view_df(casen_reg,max.len = 50)

# 3. Asociacion entre variables # ----
casen_reg <- casen_reg %>% select(participacion_mercado_laboral, sexo, educ, nacionalidad, pueblo_originario)

# Convertir factores a numéricos si tiene sentido hacerlo
casen_reg <- data.frame(lapply(casen_reg, function(x) {
  if (is.factor(x)) as.numeric(as.factor(x)) else x
}))

# Seleccionar solo las columnas numéricas
casen_reg_numeric <- casen_reg[sapply(casen_reg, is.numeric)]

# Calcular la matriz de correlación utilizando solo las observaciones completas
M <- cor(casen_reg_numeric, use = "complete.obs")

# Ver el resultado
print(M)


M <- cor(casen_reg, use = "complete.obs") # Usar solo casos con observaciones completas
diag(M) = NA # Elimina la diagonal (correlaciones absolutas de cada variable consigmo misma)
rownames(M) <- c("A. Participacion en el mescado laboral",
                 "B. Sexo",
                 "C. Educación",
                 "D. Nacionalidad",
                 "E. Pertenece a pueblo originario")
colnames(M) <-c("(A)", "(B)","(C)", "(D)", "(E)")

corrplot::corrplot(M,
                   method = "color", # Cambia los círculos por color completo de cada cuadrante
                   addCoef.col = "#000390", # Color de los coeficientes
                   type = "upper", # Deja solo las correlaciones de arriba
                   tl.col = "black", # COlor letras, rojo por defecto
                   na.label = "-")

## 3.1 Nos aseguramos que las variables sean categoricas ## ----
casen_reg$educ <- as_factor(casen_reg$educ)
casen_reg$sexo <- as_factor(casen_reg$sexo)
casen_reg$nacionalidad <- as_factor(casen_reg$nacionalidad)
casen_reg$pueblo_originario <- as_factor(casen_reg$pueblo_originario)

# 4. Regresion # ----
casen_reg <- na.omit(casen_reg)
reg1 <- lm(participacion_mercado_laboral ~ 1, data=casen_reg)
stargazer(reg1, type="text")

## 4.1 Regrasion lineal multiple ## ----
reg2<- lm(participacion_mercado_laboral ~ sexo, data = casen_reg)
reg3 <- lm(participacion_mercado_laboral ~ sexo + educ, data = casen_reg)
reg4 <- lm(participacion_mercado_laboral ~ sexo + educ + nacionalidad, data = casen_reg)
reg5 <- lm(participacion_mercado_laboral ~ sexo + educ + nacionalidad + pueblo_originario, data = casen_reg)

knitreg(list(reg2, reg3, reg4, reg5), 
        custom.model.names = c("Modelo 1",
                               "Modelo 2",
                               "Modelo 3",
                               "Modelo 4"),
        custom.note = "*** p < 0.001; ** p < 0.01; * p < 0.05",
        custom.coef.names = c("Intercepto", 
                              "Mujer <br> <i>(Ref. Hombre)</i>",
                              "Educacion básica <br> <i>(Ref. Sin estudios)</i>",
                              "Educación media", 
                              "Educación superior",
                              "Postgrados",
                              "Nacionalidad extranjera <br> <i>(Ref. Nacionalidad chilena)</i>",
                              "Pertenece puablo originario <br> <i>(Ref. No pertenece pueblo originario)</i>"),
        caption = "Participacion en el mercado laboral",
        caption.above = TRUE)

# 5. Calculo valores predichos # ----

## 5.1 Valores predichos reg2 ##
knitreg(list(reg2), 
        custom.model.names = c("Modelo 1"),
        custom.coef.names = c("Intercepto",
                              "Sexo (mujer=1)"))

## 5.2 Valores predichos reg3 ##
knitreg(list(reg3), 
        custom.model.names = c("Modelo 2"),
        custom.coef.names = c("Intercepto",
                              "Sexo (mujer=1)",
                              "Sin estudios <br> <i>(Ref. Ed. básica)</i>",
                              "Educación media", 
                              "Educación superior",
                              "Postgrados)"))

## 5.3 Valores predichos reg4 ##
knitreg(list(reg4), 
        custom.model.names = c("Modelo 3"),
        custom.coef.names = c("Intercepto",
                              "Sexo (mujer=1)",
                              "Sin estudios <br> <i>(Ref. Ed. básica)</i>",
                              "Educación media", 
                              "Educación superior",
                              "Postgrados)",
                              "Nacionalidad extranjera <br> <i>(Ref. Nacionalidad chilena)</i>"))

## 5.4 Valores predichos reg5 ##
knitreg(list(reg5), 
        custom.model.names = c("Modelo 4"),
        custom.coef.names = c("Intercepto",
                              "Sexo (mujer=1)",
                              "Sin estudios <br> <i>(Ref. Ed. básica)</i>",
                              "Educación media", 
                              "Educación superior",
                              "Postgrados)",
                              "Nacionalidad extranjera <br> <i>(Ref. Nacionalidad chilena)</i>",
                              "Pertenece puablo originario <br> <i>(Ref. No pertenece pueblo originario)</i>"))
