#librerías para cargar la data 
library(readr)
library(readxl)

#librerías para exploración y limpieza
library(ggplot2)
library(tidyr)
library(dplyr)

raw_data <- read_excel("6_penguins_lter.xlsx", col_names = FALSE)

penguins <- read.csv(text = paste(raw_data[[1]], collapse = "\n"),
                     stringsAsFactors = FALSE)

#valores de body mass
penguins[["Body.Mass..g."]]

#valores nulos de body mass
sum(is.na(penguins[["Body.Mass..g."]])) #342 de 344 registros completos 

# ================================ #
 # Análisis exploratorio de datos #
# ================================ #

# "==== estructura del data-set ===== \n"
glimpse(penguins)
#====================================================

# "==== dimensiones del data-set ===== \n"
dim(penguins)
#====================================================

# "==== nombres de las columnas/variables ===== \n"
names(penguins)
#====================================================

# "==== primeros 10 registros ===== \n"
head(penguins, 10)
#====================================================

# "==== valores faltantes ===== \n"
colSums(is.na(penguins))
#====================================================

# "==== resumen estadístico ===== \n"
summary(penguins)

# lo que muestra:
#   resumen genérico de:
#   Variables numéricas: Muestra el valor mínimo, el primer cuartil (25%),
#   la mediana, la media, el tercer cuartil (75%) y el valor máximo.
#   También avisa si hay datos perdidos (NA).Variables categóricas o factores:
#   Cuenta la frecuencia de cada categoría o nivel.Modelos estadísticos (ej. lm):
#   Presenta los coeficientes, el error estándar, los valores p y métricas como el R².
#====================================================

