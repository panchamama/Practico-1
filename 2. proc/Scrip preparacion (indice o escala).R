# Preparacion Correlacion de variables  #
# Francisca Apablaza #
# Inequidad de genero en el mercado laboral #


## Cargar paquetes ## ----
pacman::p_load(tidyverse,
               car, 
               haven,
               summarytools, 
               sjmisc,
               psych     
)

options(scipen = 999) 
rm(list = ls()) 

## Cargar base de datos rocodificada ##
load("1. input/CASEN_record.rdata")
CASEN <- proc_base
dim(CASEN)
View(CASEN)
names(CASEN)

view(dfSummary(CASEN, headings=FALSE, graph.col = FALSE)) 




