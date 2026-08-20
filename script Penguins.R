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

#Depurar registros problemáticos en Sex
penguins <- penguins %>%
  mutate(
    Sex = na_if(Sex, ""),   
    Sex = na_if(Sex, ".")   
  )

penguins %>%
  count(Sex, name = "n")

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
modelo <- lm(Body.Mass..g. ~ Flipper.Length..mm.,
             data = penguins)

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

#combinando boxplot y violin
#el gráfico de violín permite visualizar cómo están distribuidos los datos alrededor de
#cierto cuartil, minimo o máximo
#https://mode.com/blog/violin-plot-examples
penguins_cleanNoNulls <- penguins %>% filter(!is.na(Sex))
ggplot(penguins_cleanNoNulls, aes(x = Sex, y = `Flipper.Length..mm.`, fill = Sex)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.2, fill = "white", alpha = 0.8) +
  facet_wrap(~ Species) +
  labs(
    title = "Longitud de aleta por género y especie",
    x = "Género", 
    y = "Longitud de aleta (mm)",
    fill = "Género"
  ) +
  scale_fill_manual(values = c("FEMALE" = "#FF6B9D", "MALE" = "#4A90E2")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 9)
  )
  

#============= SUPUESTOS =========== #
residuos_base <- residuals(modelo)
ajustados_base <- fitted(modelo)
# ======================= 1.linealidad: =======================
# Garantizar que el modelo representa bien la relación real entre flipper length y body mass.
#E[Y | X = x] = β₀ + β₁
plot(penguins$Flipper.Length..mm., penguins$Body.Mass..g.,
     main = "Linealidad: Flipper Length vs Body Mass",
     xlab = "Flipper Length (mm)",
     ylab = "Body Mass (g)",
     pch = 19, col = rgb(0, 0, 0, 0.5))
abline(modelo, col = "red", lwd = 2)

# ======================= 2. media cero del error =======================
#Si el errormtuviera media ≠ 0, significa que β₀ está mal calibrado
#E[εᵢ] = 0
#media de los residuos
media_residuos_base <- mean(residuos_base)
print(media_residuos_base)
#prácticamente cero pero sale [1] -2.871907e-14 por redondeos de la pc

#prueba t para residuos
t.test(residuos_base, mu = 0)
#gráficamente
plot(residuos_base, main = "Residuos del modelo",
     ylab = "Residuos", xlab = "Índice",
     abline(h = 0, col = "red", lwd = 2))

#gráfica: residuos base vs ajustados
g1 <- ggplot(data.frame(ajustados_base, residuos_base), 
             aes(x = ajustados_base, y = residuos_base)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "red", method = "loess") +
  labs(title = "Residuos vs Valores Ajustados - global",
       x = "Valores Predichos", y = "Residuos") +
  theme_minimal()

g2 <- ggplot(data.frame(residuos_base), aes(sample = residuos_base)) +
  stat_qq(alpha = 0.6, color = "steelblue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot-global", x = "Cuantiles Teóricos", y = "Cuantiles Muestra") +
  theme_minimal()
grid.arrange(g1, g2, ncol = 2)
#se observa una ligera curvatura U en la gráfica de valores residuales vs ajustados
#posible relación no lineal


# ======================= 3. Varianza residual constante (homocedasticidad) ======================= #
#Asegura que las estimaciones de β₁ y sus IC no están sesgados
#Var(εᵢ) = σ² ∀ i
bptest(modelo)
plot(modelo, which = 3)


# ======================= 4. Independencia de errores entre obeservaciones =======================
# Si hay autocorr., los EE se subestiman ylos p-values son inválidos.
#Cov(εᵢ, εⱼ) = 0 (i ≠ j)
dwtest(modelo)
plot(residuos_base, type = "l", main = "Residuos secuenciales",
     ylab = "Residuos", xlab = "Observación")
abline(h = 0, col = "red", lwd = 2)


# ======================= 5. normalidad =======================
#Necesaria solo para pruebas e IC con muestras pequeñas (n < 30).
#εᵢ ~ N(0, σ²)
#uso de shapiro wilk
shapiro.test(residuos_base)
plot(modelo, which = 2)
hist(residuos_base, breaks = 25, freq = FALSE, 
     main = "Histograma de residuos",
     xlab = "Residuos", ylab = "Densidad")




# ======= PLAN B ====== 
#debido a la ligera curvatura en la gráfica de residuos vs valores ajustados
#voy a studentizar los residuos del modelo base
residuos_student_base <- rstudent(modelo)

g3 <- ggplot(data.frame(ajustados_base, residuos_student_base), 
             aes(x = ajustados_base, y = residuos_student_base)) +
  geom_point(alpha = 0.6, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "red", method = "loess") +
  labs(title = "residuos studentizados vs Valores Ajustados - studentizado",
       x = "Valores Predichos", y = "Residuos") +
  theme_minimal()

g4 <- ggplot(data.frame(residuos_student_base), aes(sample = residuos_student_base)) +
  stat_qq(alpha = 0.6, color = "black") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot-studentizado", x = "Cuantiles Teóricos", y = "Cuantiles Muestra") +
  theme_minimal()

grid.arrange(g3, g4, ncol = 2)


datos_modelo <- na.omit(penguins[, c("Body.Mass..g.", "Flipper.Length..mm.", "Species")])
#como se perdían las especies hay que reconvertir a factor
datos_modelo$Species <- as.factor(datos_modelo$Species)
nrow(datos_modelo)  # Debe ser 342
length(residuals(modelo))  # Debe ser 342

#data-set con 342 observaciones útiles planteadas inicialmente
datos_modelo$residuos <- residuals(modelo)
datos_modelo$ajustados <- fitted(modelo)

plot(datos_modelo$ajustados, datos_modelo$residuos,
     col = as.factor(datos_modelo$Species),
     main = "Residuos Modelo Base\n(coloreado por Species)",
     xlab = "Valores Predichos", 
     ylab = "Residuos",
     pch = 16,  # Puntos más visibles
     cex = 1.2)
abline(h = 0, col = "black", lty = 2, lwd = 2)

legend("topright", 
       legend = levels(datos_modelo$Species),
       col = 1:nlevels(as.factor(datos_modelo$Species)),
       pch = 16,
       cex = 0.9,
       title = "Species")

#con esto confirmamos que el efecto U de la gráfica de los residuos vs ajustados
#se debe a el hecho de sobreponer 3 grupos con diferentes interceptos y rangos de
#predicción, se sugiere que la variable "Species" sea includia en el modelo


#modelo ancova:
#1: sin interacción:
#modelo clásico que combina un factor categórico y una variable cuantitativa 
#(covariable) para explicar una variable dependiente, asumiendo que el efecto
#de la covariable es idéntico en todos los grupos

modelo_ancova_no_interaction <- lm(Body.Mass..g. ~ Flipper.Length..mm. + Species,
                    data = penguins_cleanNoNulls)


cat("\n>>> PRUEBAS DE SUPUESTOS: ANCOVA SIN INTERACCIÓN <<<\n\n")

# 1. Shapiro-Wilk (normalidad)
cat("1. TEST DE SHAPIRO-WILK (Normalidad):\n")
sw_test <- shapiro.test(residuos_modelo_ancova)
print(sw_test)
cat("   Conclusión:", if (sw_test$p.value >= 0.05) {
  "Residuos aproximadamente normales\n"
} else {
  "Desviación de normalidad (p < 0.05)\n"
})

# 2. Breusch-Pagan (homocedasticidad)
cat("\n2. TEST DE BREUSCH-PAGAN (Homocedasticidad):\n")
bp_test <- bptest(modelo_ancova_no_interaction)
print(bp_test)
cat("   Conclusión:", if (bp_test$p.value >= 0.05) {
  "Varianza aproximadamente constante\n"
} else {
  "Heterocedasticidad detectada (p < 0.05)\n"
})

# 3. Durbin-Watson (independencia)
cat("\n3. TEST DE DURBIN-WATSON (Independencia):\n")
dw_test <- dwtest(modelo_ancova_no_interaction)
print(dw_test)
cat("   Conclusión:", if (dw_test$p > 0.05) {
  "Residuos aproximadamente independientes\n"
} else {
  " Posible autocorrelación (p < 0.05)\n"
})


cat("\nEcuación estimada:\n")
print(summary(modelo_ancova_no_interaction))

cat("\nEcuación estimada: modelo convencional \n")
print(summary(modelo))

#residuos y valores ajustados nuevos:
residuos_modelo_ancova <- residuals(modelo_ancova_no_interaction)
ajustados_modelo_ancova <- fitted(modelo_ancova_no_interaction)

#graficos de los residuos del modelo ancova sin interacción:
g5 <- ggplot(data.frame(ajustados_modelo_ancova, residuos_modelo_ancova), 
             aes(x = ajustados_modelo_ancova, y = residuos_modelo_ancova)) +
  geom_point(alpha = 0.6, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "red", method = "loess") +
  labs(title = "residuos studentizados vs Valores Ajustados - modelo ancova",
       x = "Valores Predichos", y = "Residuos") +
  theme_minimal()

g6 <- ggplot(data.frame(residuos_modelo_ancova), aes(sample = residuos_modelo_ancova)) +
  stat_qq(alpha = 0.6, color = "black") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot-ancova", x = "Cuantiles Teóricos", y = "Cuantiles Muestra") +
  theme_minimal()

grid.arrange(g5, g6, ncol = 2)

#voy a intentar studentizar los residuos ancova a ver que sale:
residuos_student_ancova <- rstudent(modelo_ancova_no_interaction)

g7 <- ggplot(data.frame(ajustados_modelo_ancova, residuos_student_ancova), 
             aes(x = ajustados_modelo_ancova, y = residuos_student_ancova)) +
  geom_point(alpha = 0.6, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "red", method = "loess") +
  labs(title = "residuos studentizados vs Valores Ajustados - modelo ancova",
       x = "Valores Predichos", y = "Residuos") +
  theme_minimal()

g8 <- ggplot(data.frame(residuos_student_ancova), aes(sample = residuos_student_ancova)) +
  stat_qq(alpha = 0.6, color = "black") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot-ancova", x = "Cuantiles Teóricos", y = "Cuantiles Muestra") +
  theme_minimal()

grid.arrange(g7, g8, ncol = 2)

#lo único que hace studentizar es cambiar la escala, por ahora mejoró un poco la curva, pero hay que seguir
#trabajando en minimizarla
#este modelo presenta problemas de heterocedasticidad, seguir probando con ancova CON interacción.
#si eso no funciona, ajústate al modelo base


#modelo ancova con interacción:
#Tres líneas de regresión no paralelas
#Cada especie tiene su propia pendiente y su propio intercepto
#Las diferencias entre especies varían según el valor de Flipper Length
#La relación lineal puede tener diferente intensidad en cada especie

#"El efecto del largo de la aleta sobre la masa corporal depende de la
#  especie; algunas especies pueden ser más sensibles al cambio en la longitud de la aleta."
modelo_ancova_interaction <- lm(Body.Mass..g. ~ Flipper.Length..mm. * Species,
                                data = penguins_cleanNoNulls)

residuos_ancova_interaction <- residuals(modelo_ancova_interaction)
ajustados_ancova_interacion <- fitted(modelo_ancova_interaction)

cat("\n>>> PRUEBAS DE SUPUESTOS: ANCOVA CON INTERACCIÓN <<<\n\n")
# 1. Shapiro-Wilk (normalidad)
cat("1. TEST DE SHAPIRO-WILK (Normalidad):\n")
sw_test_interaction <- shapiro.test(residuos_ancova_interaction)
print(sw_test_interaction)
cat("   Conclusión:", if (sw_test_interaction$p.value >= 0.05) {
  "Residuos aproximadamente normales\n"
} else {
  "Desviación de normalidad (p < 0.05)\n"
})

# 2. Breusch-Pagan (homocedasticidad)
cat("\n2. TEST DE BREUSCH-PAGAN (Homocedasticidad):\n")
bp_test_interaction <- bptest(modelo_ancova_interaction)
print(bp_test_interaction)
cat("   Conclusión:", if (bp_test_interaction$p.value >= 0.05) {
  "Varianza aproximadamente constante\n"
} else {
  "Heterocedasticidad detectada (p < 0.05)\n"
})

# 3. Durbin-Watson (independencia)
cat("\n3. TEST DE DURBIN-WATSON (Independencia):\n")
dw_test_interaction <- dwtest(modelo_ancova_interaction)
print(dw_test_interaction)
cat("   Conclusión:", if (dw_test_interaction$p > 0.05) {
  "Residuos aproximadamente independientes\n"
} else {
  " Posible autocorrelación (p < 0.05)\n"
})
#no supera la prueba de normalidad por muy poco, intentar studentizar o transformar logarítmicamente a ver que pasa
#podríamos intentar amputar valores o residuos extremos
#graficos de los residuos del modelo ancova con interacción:
g9 <- ggplot(data.frame(ajustados_ancova_interacion, residuos_ancova_interaction), 
             aes(x = ajustados_ancova_interacion, y = residuos_ancova_interaction)) +
  geom_point(alpha = 0.6, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "red", method = "loess") +
  labs(title = "residuos vs Valores Ajustados - modelo ancova interacción",
       x = "Valores Predichos", y = "Residuos") +
  theme_minimal()

g10 <- ggplot(data.frame(ajustados_ancova_interacion), aes(sample = residuos_ancova_interaction)) +
  stat_qq(alpha = 0.6, color = "black") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot-ancova con interacción", x = "Cuantiles Teóricos", y = "Cuantiles Muestra") +
  theme_minimal()

grid.arrange(g9, g10, ncol = 2)

#la curvatura casi no se aprecia en la gráfica residual 1, o al menos es menos pronunciada que la primera
#el qq plot carece de nornmalidad en las colas. Intentaré arreglarlo con log

modelo_ancova_interaction_log <- lm(
  log(Body.Mass..g.) ~ Flipper.Length..mm. * Species,
  data = datos_modelo
)

residuos_log_ancova_interaction <- residuals(modelo_ancova_interaction_log)
ajustados_log_ancova_interaction <- fitted(modelo_ancova_interaction_log)

cat("========== SUPUESTOS: ANCOVA CON INTERACCIÓN (LOG) ==========\n\n")

# 1. Shapiro-Wilk
sw_log <- shapiro.test(residuos_log_ancova_interaction)
cat("1. SHAPIRO-WILK (Normalidad):\n")
print(sw_log)
cat("   Conclusión:", if (sw_log$p.value >= 0.05) {
  "Normalidad satisfecha\n"
} else {
  "Desviación de normalidad\n"
}, "\n")

# 2. Breusch-Pagan
cat("\n2. BREUSCH-PAGAN (Homocedasticidad):\n")
bp_log <- bptest(modelo_ancova_interaction_log)
print(bp_log)
cat("   Conclusión:", if (bp_log$p.value >= 0.05) {
  "Homocedasticidad satisfecha\n"
} else {
  "Heterocedasticidad detectada\n"
}, "\n")

# 3. Durbin-Watson
cat("\n3. DURBIN-WATSON (Independencia):\n")
dw_log <- dwtest(modelo_ancova_interaction_log)
print(dw_log)
cat("   Conclusión:", if (dw_log$p.value >= 0.05) {
  "Independencia satisfecha\n"
} else {
  "Posible autocorrelación\n"
}, "\n")

#valor p para la prueba de breusch pragan = p-value = 0.0001137
#heterocedasticidad detectada, no sirve

g12 <- ggplot(data.frame(residuos_log_ancova_interaction), aes(sample = residuos_log_ancova_interaction)) +
  stat_qq(alpha = 0.6, color = "black") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot-ancova con interacción - logaritmos", x = "Cuantiles Teóricos", y = "Cuantiles Muestra") +
  theme_minimal()

g11 <- ggplot(data.frame(ajustados_log_ancova_interaction, residuos_log_ancova_interaction), 
             aes(x = ajustados_log_ancova_interaction, y = residuos_log_ancova_interaction)) +
  geom_point(alpha = 0.6, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "red", method = "loess") +
  labs(title = "residuos vs Valores Ajustados - modelo ancova interacción - logaritmo",
       x = "Valores Predichos", y = "Residuos") +
  theme_minimal()

grid.arrange(g11, g12, ncol = 2)

#el problema con este modelo es que no pasa la prueba de heterocedasticidad
#regresar al modelo ancova base