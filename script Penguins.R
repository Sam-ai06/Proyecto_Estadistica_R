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

# ================================ #
 # Análisis exploratorio de datos #
# ================================ #

# checklist: agregar una respuesta cuando se haya definido en la asignación del av

# ¿Cuál será la variable variable objetivo (Y)? (cuantitativa continua)
# ¿Cuál será la variable predictora (X)? (cuantitativa)
# ¿Cuál será la  variable de agrupación? (categórica)
# ¿Hay relación lineal visible entre X e Y?
# ¿Cuántos datos faltantes tengo y en qué variables?
# ¿Cuál es la justificación teórica para elegir estas variables?

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

#valores de body mass
penguins[["Body.Mass..g."]]
#====================================================

#valores nulos de body mass
sum(is.na(penguins[["Body.Mass..g."]])) #342 de 344 registros completos 
#====================================================

#filas duplicadas
filasDuplicadas <- sum(duplicated(penguins))
cat("Número de filas duplicadas en el data-set: ", filasDuplicadas)
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

#histograma, masa corporal
hist(penguins$Body.Mass..g.,
     breaks = 30,
     main = "masa corporal(g)")

#boxplot: masa del cuerpo
boxplot(penguins$Body.Mass..g.,
        xlab = "masa corporal en gramos",
        horizontal = TRUE,
        main = "variabilidad de la masa corporal de los pinguinos (g)",
        col = "cyan")

#boxplot: variabilidad de la masa corporal en gramos por especie
#uso de GGplot pq no me permitía colocar de forma nativa una grilla en el plot.
ggplot(penguins, aes(x = Species, y = `Body.Mass..g.`, fill = Species)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Adelie", "Chinstrap", "Gentoo")) +
  theme_minimal() +
  labs(title = "Variabilidad en la masa corporal por especie de pingüino (g)",
       x = "Especie",
       y = "variabilidad de la masa corporal - g")

#=========================== ALETAS - PINGUINOS ====================================================
#¿hay valores nulos en esta colmuna?
valoresNulosAletas <- sum(is.na(penguins[["Flipper.Length..mm."]])) #342 de 344 registros completos 
cat("valores nulos para la longitud de las aletas de los pinguinos (mm): ", valoresNulosAletas)

#histograma: largo de las aletas (mm)
hist(penguins$Flipper.Length..mm.,
     breaks = 30,
     main ="largo de las aletas (g)",
     col = "green")

#boxplot: variabilidad en el largo de las aletas
boxplot(penguins$Flipper.Length..mm.,
        xlab = "largo de las aletas - (mm)",
        main = "variabilidad en el largo de las aletas de las 3 especies",
        horizontal = TRUE,
        col = "cyan")

#boxplot: variabilidad del largo de las aletas por especie
#uso de GGplot pq no me permitía colocar de forma nativa una grilla en el plot.
ggplot(penguins, aes(x = Species, y = `Flipper.Length..mm.`, fill = Species)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Adelie", "Chinstrap", "Gentoo")) +
  theme_minimal() +
  labs(title = "Variabilidad en el largo de las aletas por especie de pingüino (mm)",
       x = "Especie",
       y = "Largo de aleta (mm)")

#scatterplot: masa corporal - largo de las aletas
plot(penguins$Body.Mass..g., penguins$Flipper.Length..mm.,
     xlab = "masa corporal de pinguinos -g", ylab = "largo de las aletas - mm",
     main = "relación entre masa corporal(g) y largo de la aletas (mm)",
     col = "red",
     pch = 21,
     bg = "black",)

#barplot para visualizar categóricos
barplot(table(penguins$Species),
        main = "Especies de pinguinos",
        names.arg = c("Adelie", "Chinstrap", "Gentoo"),
        las = 2)

#============= SUPUESTOS =========== #
#Coeficiente R: o coeficiente de correlación de pearson,
#mide la fuerza y la dirección de la relación lineal entre
#dos variables cuantitativas. Su valor va desde -1 hasta 1,
#indicando si los cambios en una variable se acompañan de cambios proporcionales en la otra.
#entonces, un valor más cercano a 1 indica una fuerte correlación entre ambas variables

valorCorrelacion <- cor(penguins$Flipper.Length..mm.,
                        penguins$Body.Mass..g.,
                        use = "complete.obs")

#coeficiente R entre masa corporal y culmen length
valorCorrelacion2 <- cor(penguins$Culmen.Length..mm.,
                         penguins$Body.Mass..g.,
                         use = "complete.obs")
cat("valor R entre largo de las aletas y masa corporal en pinguinos: ", valorCorrelacion, "\n")
cat("valor R entre el largo de la parte superior del pico y masa corporal en pinguinos: ", valorCorrelacion2)


# Linealidad de la relación entre body mass y flipper length:
#VERIFICADO: linea 85
  
#======= POR COMPROBAR ======= #
#Normalidad de residuales: deben estar distribuidos normalmente
#homocedasticidad o varianza constante: varianza de los residuales constante
#Independencia de residuales: los residuales no deben estar autocorrelacionados
#pendiente ANOVA para coeficiente R con variables categóricas


