#si no están isntaladas ejecuta esto
librerias <- c("readr", "readxl", "ggplot2", "tidyr", "dplyr", "lmtest", "gridExtra")
for (lib in librerias) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
    library(lib, character.only = TRUE)
  }
}

#librerías para cargar la data 
library(readr)
library(readxl)

#librerías para exploración y limpieza
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)

#lmtest: contiene diferentes pruebas para verificar asunciones como homocedasticidad, independencia, etc.
library(lmtest)


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



# ===== verficicación de supuestos ====== #
# Linealidad de la relación entre body mass y flipper length:
#scatterplot: masa corporal - largo de las aletas
plot(penguins$Body.Mass..g., penguins$Flipper.Length..mm.,
     xlab = "masa corporal de pinguinos -g", ylab = "largo de las aletas - mm",
     main = "relación entre masa corporal(g) y largo de la aletas (mm)",
     col = "red",
     pch = 21,
     bg = "black",)

# =========== Normalidad de Residuos =========== #
# Done: hacer test con shapiro-wilk: Shapiro-Wilk: p-valor = 0.XXX → [Se cumple / No se cumple]
#shapiro wilk comprueba que los residuos del modelo sigan una distribución normal
modelo <- lm(Body.Mass..g. ~ Flipper.Length..mm.,
             data = penguins)

residuos <- residuals(modelo)
ajustados <- fitted(modelo)
valorShapiroTest <- shapiro.test(residuos)
valorP <- valorShapiroTest$p.value
print(shapiro.test(residuos))

summary(modelo)
#Done: mostrar por pantalla el valor p del test
cat("Valor p del test de Shapiro-Wilk:", valorShapiroTest$p.value, "\n")
if(valorP > 0.05){
  cat("los residuos siguen una distribución normal \n")
} else{
  cat("los residuos no siguen una distribución normal \n")
}

# =========== done: homocedasticidad: prueba de breusch - pagan =========== # 
pruebaBP <- bptest(modelo)
print(pruebaBP)
# Homocedasticidad: prueba de Breusch-Pagan, p-valor = 0.1418
# No se rechaza la hipótesis de homocedasticidad de los residuos.

# ================ hecho: prueba de independencia: durbin-watson ================ #
#La hipótesis nula para la prueba de Durbin-Watson es que no hay autocorrelación en los residuos (son independientes entre sí)
#https://www.geeksforgeeks.org/r-language/understanding-durbin-watson-test-in-r/
pruebaWatson <- dwtest(modelo)
print(pruebaWatson)
if(pruebaWatson$p.value>0.05){
  print("no existe evidencia estadística suficiente para rechazar la hipótesis nula que establece que los residuos no están autocorrelacionados (o sea que son independientes entre sí)")
}else{
  print("los residuos están autocorrelacionados, no se puede continuar.")
}
# ====== supuestos completados ======= #

# ========== gráficas de residuos =========== #
# https://blog.minitab.com/es/blog/analisis-de-regresion-como-puedo-interpretar-el-r-cuadrado-y-evaluar-la-bondad-de-ajuste
# https://blog.minitab.com/en/blog/adventures-in-statistics-2/why-you-need-to-check-your-residual-plots-for-regression-analysis

        #En general, un modelo se ajusta bien a los datos si las diferencias entre los valores 
        #observados y los valores de predicción del modelo son pequeñas y no presentan sesgo.

        #en la fórmula de regresión lineal simple: El error es la diferencia entre el valor
        #esperado y el valor observado (epsilon).Captura el "ruido" o variación natural de los
        #datos que la línea no puede explicar y otras funciones más

# Gráfico 1: Residuos vs Ajustados
g1 <- ggplot(data.frame(ajustados, residuos), 
             aes(x = ajustados, y = residuos)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "red", method = "loess") +
  labs(title = "Residuos vs Valores Ajustados - global",
       x = "Valores Predichos", y = "Residuos") +
  theme_minimal()

# Gráfico 2: Q-Q Plot
g2 <- ggplot(data.frame(residuos), aes(sample = residuos)) +
  stat_qq(alpha = 0.6, color = "steelblue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot-global", x = "Cuantiles Teóricos", y = "Cuantiles Muestra") +
  theme_minimal()

#merged
grid.arrange(g1, g2, ncol = 2)


#se observa subestimación en ciertas áreas de el primer gráfico (~3000g)
#la relación entre body mass y flipper length podría no ser completamente lineal
#esto debido a la ligera curvatura que presenta el scatterplot

#q-q plot muestra una relación aparentemente lineal
#presentan ligeras desviaciones en las colas

# ============================================================================ #
#para arreglar la curvatura residual se puede usar transformación logarítmica
#https://rpubs.com/juanjo_edm/1091363
#https://www.medcalc.org/es/manual/log-transformation.php

#1. variables logarítimicas
penguins_limpio <- penguins %>%
  drop_na(`Body.Mass..g.`, `Flipper.Length..mm.`) %>%
  mutate(
    log_body_mass = log(`Body.Mass..g.`),
    log_flipper_length = log(`Flipper.Length..mm.`)
  )

#2. modelo ajustado con logaritmo
modelo_logaritmo <- lm(log_body_mass ~ log_flipper_length, data = penguins_limpio)
residuos_log <- residuals(modelo_logaritmo)
ajustados_log <-fitted(modelo_logaritmo)

#3. gráficas nuevas
# Gráfico : Residuos vs Ajustados
g3 <- ggplot(data.frame(ajustados_log, residuos_log), 
             aes(x = ajustados_log, y = residuos_log)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "red", method = "loess") +
  labs(title = "Residuos vs Valores Ajustados luego de la transformación logarítmica",
       x = "Valores Predichos", y = "Residuos") +
  theme_minimal()

# Gráfico 4: Q-Q Plot
g4 <- ggplot(data.frame(residuos_log), aes(sample = residuos_log)) +
  stat_qq(alpha = 0.6, color = "steelblue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot - trans. logarítmica", x = "Cuantiles Teóricos", y = "Cuantiles Muestra") +
  theme_minimal()

#merged
grid.arrange(g3, g4, ncol = 2)

#se puede observar que la curvatura persiste, pero es algo más suave
#al no desaparecer, hay posilidades que considerar:
  #el modelo ignora por ahora que existen 3 especies diferentes cuyos patrones de crecimiento
  #podrían ser diferentes
  #hipótesis: cada especie tiene su propia relación lineal, que al mezclarse resulta en esa curva
  #al omitir la variable sex se introduce heterogeneidad
















# == no prestar atención por ahora === #
#modelo con especies separadas (opción B)
#el modelo anterior considera las especies de forma glogal sin considerar su especie o sexo
#propongo crear modelos para cada especie de pinguino, por ahora solo para comparar residuales e interceptos
#el documento pide el uso de la variable categórica de dos niveles, pero aún no sé como implementarla al proyecto

#1. separación de datos por especie:
adelie_data <- penguins %>% filter(Species == "Adelie Penguin (Pygoscelis adeliae)")
gentoo_data <- penguins %>% filter(Species == "Gentoo penguin (Pygoscelis papua)")
chinstrap_data <- penguins %>% filter(Species == "Chinstrap penguin (Pygoscelis antarctica)")

#2. creación de modelo por especie de pinguino:
modelo_adelie <- lm(Body.Mass..g. ~ Flipper.Length..mm., data = adelie_data)
modelo_gentoo <- lm(Body.Mass..g. ~ Flipper.Length..mm., data = gentoo_data)
modelo_chinstrap <- lm(Body.Mass..g. ~ Flipper.Length..mm., data = chinstrap_data)

#3. resultados por modelo
summary(modelo_adelie)
summary(modelo_gentoo)
summary(modelo_chinstrap)

#residuos y ajustados por modelo
residuos_adelie <- residuals(modelo_adelie)
ajustados_adelie <- fitted(modelo_adelie)

residuos_gentoo <- residuals(modelo_gentoo)
ajustados_gentoo <- fitted(modelo_gentoo)

residuos_chinstrap <- residuals(modelo_chinstrap)
ajustados_chinstrap <- fitted(modelo_chinstrap)
#==========================================================================================#
# gráficas nuevas por modelo
# Gráfico : Residuos vs Ajustados:adelie
Grafica_adelie <- ggplot(data.frame(ajustados_adelie, residuos_adelie), 
             aes(x = ajustados_adelie, y = residuos_adelie)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "red", method = "loess") +
  labs(title = "Residuos vs Valores Ajustados-adelie",
       x = "Valores Predichos", y = "Residuos") +
  theme_minimal()

#Q-Q plot: adelie
QQ_adelie <- ggplot(data.frame(residuos_adelie), aes(sample = residuos_adelie)) +
  stat_qq(alpha = 0.6, color = "steelblue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot-adelie", x = "Cuantiles Teóricos", y = "Cuantiles Muestra") +
  theme_minimal()

grid.arrange(Grafica_adelie, QQ_adelie, ncol = 2)

#=========================================================================================#
# Gráfico : Residuos vs Ajustados:gentoo
Grafica_gentoo <- ggplot(data.frame(ajustados_gentoo, residuos_gentoo), 
                         aes(x = ajustados_gentoo, y = residuos_gentoo)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "red", method = "loess") +
  labs(title = "Residuos vs Valores Ajustados-gentoo",
       x = "Valores Predichos", y = "Residuos") +
  theme_minimal()

#Q-Q plot: gentoo
QQ_gentoo <- ggplot(data.frame(residuos_gentoo), aes(sample = residuos_gentoo)) +
  stat_qq(alpha = 0.6, color = "steelblue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot-gentoo", x = "Cuantiles Teóricos", y = "Cuantiles Muestra") +
  theme_minimal()

grid.arrange(Grafica_gentoo, QQ_gentoo, ncol = 2)
#========================================================================================#
# Gráfico : Residuos vs Ajustados:chinstrap
Grafica_chinstrap <- ggplot(data.frame(ajustados_chinstrap, residuos_chinstrap), 
                         aes(x = ajustados_chinstrap, y = residuos_chinstrap)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "red", method = "loess") +
  labs(title = "Residuos vs Valores Ajustados-chinstrap",
       x = "Valores Predichos", y = "Residuos") +
  theme_minimal()

#Q-Q plot: gentoo
QQ_chinstrap <- ggplot(data.frame(residuos_chinstrap), aes(sample = residuos_chinstrap)) +
  stat_qq(alpha = 0.6, color = "steelblue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot-chinstrap", x = "Cuantiles Teóricos", y = "Cuantiles Muestra") +
  theme_minimal()

grid.arrange(Grafica_chinstrap, QQ_chinstrap, ncol = 2)
#=========================================================================================#
ggplot(penguins_limpio, aes(x = log_flipper_length, y = log_body_mass, color = Species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  labs(
    title = "Regresión Log-Log: Body Mass ~ Flipper Length (por Especie)",
    x = "log(Flipper Length (mm))",
    y = "log(Body Mass (g))",
    color = "Especie"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
#comparar con gráfico log log anterior global
#tal vez la ecuación planteada para el rls necesita una variable más que separe por especie de pinguino





