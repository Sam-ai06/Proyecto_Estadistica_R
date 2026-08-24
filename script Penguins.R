# ============================================================
# PROYECTO DE ESTADÍSTICA - PENGUINS LTER
# Script refactorizado - final v1
# ============================================================

# ============================================================
# 0. PAQUETES
# ============================================================

#si no están isntaladas ejecuta esto
librerias <- c("readr", "readxl", "ggplot2", "tidyr", "dplyr", "lmtest", "gridExtra")
for (lib in librerias) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
    library(lib, character.only = TRUE)
  }
}

# ============================================================
# 1. CARGA Y DEPURACIÓN BÁSICA
# ============================================================

raw_data <- read_excel("6_penguins_lter.xlsx", col_names = FALSE)

penguins <- read.csv(
  text = paste(raw_data[[1]], collapse = "\n"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    Sex = trimws(Sex),
    Sex = na_if(Sex, ""),
    Sex = na_if(Sex, ".")
  )

# Frecuencia de la variable Sex después de la depuración
penguins %>%
  count(Sex, name = "n") %>%
  print()


# ============================================================
# 2. EXPLORACIÓN GENERAL Y CALIDAD DE DATOS
# ============================================================

cat("\n===== ESTRUCTURA DEL DATASET =====\n")
glimpse(penguins)

cat("\n===== DIMENSIONES =====\n")
print(dim(penguins))

cat("\n===== NOMBRES DE VARIABLES =====\n")
print(names(penguins))

cat("\n===== PRIMEROS 10 REGISTROS =====\n")
print(head(penguins, 10))

cat("\n===== VALORES FALTANTES POR VARIABLE =====\n")
print(colSums(is.na(penguins)))

cat("\n===== FILAS DUPLICADAS =====\n")
cat("Número de filas duplicadas:", sum(duplicated(penguins)), "\n")

cat("\n===== RESUMEN ESTADÍSTICO =====\n")
print(summary(penguins))

cat("\nRegistros faltantes en Body Mass (g):",
    sum(is.na(penguins$Body.Mass..g.)), "\n")

cat("Registros faltantes en Flipper Length (mm):",
    sum(is.na(penguins$Flipper.Length..mm.)), "\n")


# ============================================================
# 3. FUNCIONES AUXILIARES
# ============================================================

# ------------------------------------------------------------
# 3.1. Gráficos descriptivos para una variable cuantitativa
# ------------------------------------------------------------

graficar_cuantitativa <- function(datos, variable, etiqueta, unidad) {
  valores <- datos[[variable]]
  
  hist(
    valores,
    breaks = 30,
    main = paste("Histograma de", etiqueta),
    xlab = paste0(etiqueta, " (", unidad, ")")
  )
  
  boxplot(
    valores,
    horizontal = TRUE,
    main = paste("Variabilidad de", etiqueta),
    xlab = paste0(etiqueta, " (", unidad, ")")
  )
  
  #dispersión
  plot(variable,
       main = paste("Gráfico de dispersión de ", etiqueta),
       xlab = paste0(etiqueta, "(", unidad, ")")
  )
  
  grafico_especie <- ggplot(
    datos,
    aes(x = Species, y = .data[[variable]], fill = Species)
  ) +
    geom_boxplot() +
    scale_x_discrete(labels = c("Adelie", "Chinstrap", "Gentoo")) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(
      title = paste(etiqueta, "por especie de pingüino"),
      x = "Especie",
      y = paste0(etiqueta, " (", unidad, ")")
    )
  
  print(grafico_especie)
}


# ------------------------------------------------------------
# 3.2. Pruebas de supuestos para un modelo lineal
# ------------------------------------------------------------

pruebas_supuestos <- function(modelo, nombre_modelo) {
  residuos <- residuals(modelo)
  
  cat("\n============================================================\n")
  cat("PRUEBAS DE SUPUESTOS:", nombre_modelo, "\n")
  cat("============================================================\n")
  
  # Normalidad
  prueba_shapiro <- shapiro.test(residuos)
  cat("\n1. SHAPIRO-WILK - Normalidad\n")
  print(prueba_shapiro)
  cat(
    "Conclusión:",
    if (prueba_shapiro$p.value >= 0.05) {
      "No se detecta una desviación significativa de la normalidad.\n"
    } else {
      "Se detecta evidencia de desviación de la normalidad.\n"
    }
  )
  
  # Homocedasticidad
  prueba_bp <- bptest(modelo)
  cat("\n2. BREUSCH-PAGAN - Homocedasticidad\n")
  print(prueba_bp)
  cat(
    "Conclusión:",
    if (prueba_bp$p.value >= 0.05) {
      "No se detecta evidencia de heterocedasticidad.\n"
    } else {
      "Se detecta evidencia de heterocedasticidad.\n"
    }
  )
  
  # Independencia
  prueba_dw <- dwtest(modelo)
  cat("\n3. DURBIN-WATSON - Independencia\n")
  print(prueba_dw)
  cat(
    "Conclusión:",
    if (prueba_dw$p.value >= 0.05) {
      "No se detecta evidencia significativa de autocorrelación.\n"
    } else {
      "Se detecta posible autocorrelación.\n"
    }
  )
  
  invisible(
    list(
      shapiro = prueba_shapiro,
      breusch_pagan = prueba_bp,
      durbin_watson = prueba_dw
    )
  )
}


# ------------------------------------------------------------
# 3.3. Gráficos comunes de diagnóstico residual
# ------------------------------------------------------------

graficar_residuos <- function(modelo, nombre_modelo, studentizados = FALSE) {
  residuos <- if (studentizados) rstudent(modelo) else residuals(modelo)
  ajustados <- fitted(modelo)
  tipo_residuo <- if (studentizados) "studentizados" else "ordinarios"
  
  datos_diagnostico <- data.frame(
    ajustados = ajustados,
    residuos = residuos
  )
  
  grafico_residuos <- ggplot(
    datos_diagnostico,
    aes(x = ajustados, y = residuos)
  ) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(se = FALSE, method = "loess") +
    theme_minimal() +
    labs(
      title = paste("Residuos", tipo_residuo, "vs ajustados -", nombre_modelo),
      x = "Valores ajustados",
      y = "Residuos"
    )
  
  grafico_qq <- ggplot(
    datos_diagnostico,
    aes(sample = residuos)
  ) +
    stat_qq(alpha = 0.6) +
    stat_qq_line() +
    theme_minimal() +
    labs(
      title = paste("Q-Q Plot -", nombre_modelo),
      x = "Cuantiles teóricos",
      y = "Cuantiles muestrales"
    )
  
  grid.arrange(grafico_residuos, grafico_qq, ncol = 2)
}


# ============================================================
# 4. ANÁLISIS DESCRIPTIVO
# ============================================================

# Variable respuesta: Body Mass (g)
graficar_cuantitativa(
  datos = penguins,
  variable = "Body.Mass..g.",
  etiqueta = "masa corporal",
  unidad = "g"
)

# Variable explicativa: Flipper Length (mm)
graficar_cuantitativa(
  datos = penguins,
  variable = "Flipper.Length..mm.",
  etiqueta = "longitud de aleta",
  unidad = "mm"
)

# Variable categórica: Species
barplot(
  table(penguins$Species),
  main = "Especies de pingüinos",
  names.arg = c("Adelie", "Chinstrap", "Gentoo"),
  las = 2
)


#scatterplot de penguins por species


# Distribución de longitud de aleta por sexo y especie.
# Este subconjunto solo se usa para este gráfico, porque aquí sí interviene Sex.
penguins %>%
  filter(!is.na(Sex), !is.na(Flipper.Length..mm.)) %>%
  ggplot(aes(x = Sex, y = Flipper.Length..mm., fill = Sex)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.2, fill = "white", alpha = 0.8) +
  facet_wrap(~ Species) +
  theme_minimal() +
  labs(
    title = "Longitud de aleta por sexo y especie",
    x = "Sexo",
    y = "Longitud de aleta (mm)",
    fill = "Sexo"
  )

# ---------------------------------------------------------------------
# coeficiente de correlación de pearson para flipper length y body mass
# ---------------------------------------------------------------------
pearson_coefficient <- cor.test(penguins$Flipper.Length..mm., penguins$Body.Mass..g., method = "pearson")
print(pearson_coefficient)

#coeficiente bastante alto, útil para las pruebas posteriores 

# ============================================================
# 5. DATOS UTILIZADOS EN LOS MODELOS
# ============================================================

# Se crea una versión analítica sin modificar el dataset original.
# Solo se excluyen observaciones con NA en variables usadas por los modelos.
datos_modelo <- penguins %>%
  select(Body.Mass..g., Flipper.Length..mm., Species) %>%
  drop_na() %>%
  mutate(Species = factor(Species))

cat("\nObservaciones completas para los modelos:", nrow(datos_modelo), "\n")


# ============================================================
# 6. REGRESIÓN LINEAL SIMPLE - MODELO PRINCIPAL
# ============================================================

modelo_rls <- lm(
  Body.Mass..g. ~ Flipper.Length..mm.,
  data = datos_modelo
)

cat("\n===== RESUMEN DEL MODELO RLS =====\n")
print(summary(modelo_rls))

# ------------------------------------------------------------
# 6.1. Linealidad: X vs Y
# ------------------------------------------------------------

plot(
  datos_modelo$Flipper.Length..mm.,
  datos_modelo$Body.Mass..g.,
  main = "Linealidad: longitud de aleta vs masa corporal",
  xlab = "Longitud de aleta (mm)",
  ylab = "Masa corporal (g)",
  pch = 19
)
abline(modelo_rls, lwd = 2)


# ------------------------------------------------------------
# 6.2. Media de residuos
# ------------------------------------------------------------

# En una regresión con intercepto, la media residual es aproximadamente
# cero por construcción. Se muestra como comprobación descriptiva.
cat("\nMedia de los residuos del modelo RLS:",
    mean(residuals(modelo_rls)), "\n")

plot(
  residuals(modelo_rls),
  main = "Residuos del modelo RLS",
  xlab = "Índice",
  ylab = "Residuos",
  pch = 19
)
abline(h = 0, lty = 2, lwd = 2)
#residuos aproximadamente dispersos, la mayoría están cerca del cero
#se observa que ciertos residuos están en los extremos, por lo que podrían ser
#residuos atípicos


# ------------------------------------------------------------
# 6.3. Pruebas y gráficos de diagnóstico
# ------------------------------------------------------------

pruebas_supuestos(modelo_rls, "RLS")
graficar_residuos(modelo_rls, "RLS")

# ------------------------------------------------------------
# 6.3.1 gráfias residuales
# ------------------------------------------------------------

# Residuos studentizados: útiles para identificar observaciones inusuales.
graficar_residuos(modelo_rls, "RLS", studentizados = TRUE)
#observación inusual en coordenada aprox. (3.400, 3.3)
#observación inusual en coordenada aprox, (3.750, -2.8)

# Gráficos estándar de R para normalidad y homocedasticidad
plot(modelo_rls, which = 2)
plot(modelo_rls, which = 3)

# Residuos en orden de observación
plot(
  residuals(modelo_rls),
  type = "l",
  main = "Residuos secuenciales - RLS",
  xlab = "Observación",
  ylab = "Residuos"
)
abline(h = 0, lty = 2, lwd = 2)

# Histograma de residuos
hist(
  residuals(modelo_rls),
  breaks = 25,
  freq = FALSE,
  main = "Histograma de residuos - RLS",
  xlab = "Residuos",
  ylab = "Densidad"
)


# ============================================================
# 7. REVISIÓN DEL PATRÓN RESIDUAL POR ESPECIE
# ============================================================

plot(
  fitted(modelo_rls),
  residuals(modelo_rls),
  col = as.integer(datos_modelo$Species),
  main = "Residuos del modelo RLS por especie",
  xlab = "Valores ajustados",
  ylab = "Residuos",
  pch = 16,
  cex = 1.1
)
abline(h = 0, lty = 2, lwd = 2)

legend(
  "topright",
  legend = levels(datos_modelo$Species),
  col = seq_along(levels(datos_modelo$Species)),
  pch = 16,
  cex = 0.8,
  title = "Species"
)

# Si se observa una estructura residual distinta por especie, puede investigarse
# como explicación del patrón. Esto no convierte automáticamente a ANCOVA en el
# modelo principal del proyecto, cuyo enfoque sigue siendo la RLS.
# existe una curvatura residual, por lo que construir un modelo que incluya Species podría servir.

#los residuos por especie están bastante agrupados
#lo que podría explicar la curvatura en la gráfica de los residuos
#y el hecho de que a pesar de que esta relación X-Y tenga un coeficiente de 
#correlación bastante alto (.871), sus residuos estén distribuidos de esta forma

#comentario del profesor:
#la causa podría ser una variable latente, pero eso no se estudia en el curso, por lo que podemos trabajar
#con este modelo así como está

# ============================================================
# 8. ANÁLISIS DESCRIPTIVO RESTANTE
# ============================================================

# ------------------------------------------------------------
# 8.1. Resumen descriptivo de Body Mass
# ------------------------------------------------------------

datos_modelo %>%
  summarise(
    n = n(),
    media = mean(Body.Mass..g.),
    mediana = median(Body.Mass..g.),
    sd = sd(Body.Mass..g.),
    cv = sd(Body.Mass..g.) / mean(Body.Mass..g.) * 100,
    q1 = quantile(Body.Mass..g., 0.25),
    q3 = quantile(Body.Mass..g., 0.75),
    minimo = min(Body.Mass..g.),
    maximo = max(Body.Mass..g.)
  )

# ------------------------------------------------------------
# 8.2. Resumen descriptivo de Flipper Length
# ------------------------------------------------------------

datos_modelo %>%
  summarise(
    n = n(),
    media = mean(Flipper.Length..mm.),
    mediana = median(Flipper.Length..mm.),
    sd = sd(Flipper.Length..mm.),
    cv = sd(Flipper.Length..mm.) / mean(Flipper.Length..mm.) * 100,
    q1 = quantile(Flipper.Length..mm., 0.25),
    q3 = quantile(Flipper.Length..mm., 0.75),
    minimo = min(Flipper.Length..mm.),
    maximo = max(Flipper.Length..mm.)
  )

# ------------------------------------------------------------
# 8.3. Medidas descriptivas por especie - Body Mass
# ------------------------------------------------------------

datos_modelo %>%
  group_by(Species) %>%
  summarise(
    n = n(),
    media = mean(Body.Mass..g.),
    sd = sd(Body.Mass..g.),
    mediana = median(Body.Mass..g.)
  )

# ------------------------------------------------------------
# 8.4. Medidas descriptivas por especie - Flipper Length
# ------------------------------------------------------------

datos_modelo %>%
  group_by(Species) %>%
  summarise(
    n = n(),
    media = mean(Flipper.Length..mm.),
    sd = sd(Flipper.Length..mm.),
    mediana = median(Flipper.Length..mm.)
  )


# ============================================================
# 9. AJUSTE E INTERPRETACIÓN DEL MODELO
# ============================================================
intercepto_modelo_RLS <- coef(modelo_rls)[1]
pendiente_modelo_RLS <- coef(modelo_rls)[2]
# ------------------------------------------------------------
# 9.1. Ecuación estimada y coeficientes
# ------------------------------------------------------------
# DONE:
# - Mostrar beta_0 estimado.
print("\n intercepto del modelo de RLS:\n")
print(intercepto_modelo_RLS)

# - Mostrar beta_1 estimado.
print("\n pendiente del modelo de RLS: \n")
print(pendiente_modelo_RLS)  

# - Escribir la ecuación estimada de la RLS.
cat("\nEcuación estimada de la RLS:\n")
cat("ŷ =", intercepto_modelo_RLS, "+", pendiente_modelo_RLS, "* x\n")

# - Interpretar intercepto y pendiente en contexto.
print("Interpretación: Por cada milímetro adicional de longitud de aleta, la masa corporal aumenta en  B1 gramos ")
coef(modelo_rls)
resumen_modelo_rls <- summary(modelo_rls)

# ------------------------------------------------------------
# 9.2. Intervalos de confianza de los coeficientes
# ------------------------------------------------------------
# DONE:
# - Calcular IC para beta_0 y beta_1.
confint(modelo_rls, level = 0.95)
cat("Con un nivel de confianza del 95 %, se estima que por cada incremento de 1 mm
      en la longitud de la aleta, la masa corporal media de los pingüinos aumenta entre aproximadamente 46.70 g y 52.67 g. ")

cat("Con un nivel de confianza del 95 %, el intercepto poblacional βo se encuentra 
      entre X e Y gramos. Sin embargo, este parámetro representa la masa corporal 
      esperada para una longitud de aleta de 0 mm, valor que se encuentra fuera del
      rango observado y carece de una interpretación biológica pertinente. \n")
# ------------------------------------------------------------
# 9.3. Medidas de ajuste
# ------------------------------------------------------------
# DONE:
# - Reportar coeficiente de correlación lineal.
coeficiente_pearson <- cor(penguins$Flipper.Length..mm., 
                           penguins$Body.Mass..g., 
                           method = "pearson", 
                           use = "complete.obs"
                           )
print("coeficiente de correlación de pearson: \n")
print(coeficiente_pearson)

# - Reportar R^2.
# R^2 mide qué porcentaje de la variación de una variable dependiente es explicado 
#por un modelo de regresión lineal

print("R cuadrado: \n")
print(resumen_modelo_rls$r.squared) # 0.758992

# - Reportar R^2 ajustado si se considera pertinente.
print("R cuadrado ajustado: \n")
print(resumen_modelo_rls$adj.r.squared) # 0.758283
#esto quiere decir que el modelo explica alrededor del 76% de la variabilidad de Y

# - Reportar error estándar residual.
print("Error estándar residual: \n")
print(resumen_modelo_rls$sigma) # 394.278
#TODOS ESTOS VALORES DEBEN SER INTERPRETADOS LUEGO EN EL REPORTE Y EN LA EXPOCISIÓN.

# ------------------------------------------------------------
# 9.4. Significancia global de la regresión
# ------------------------------------------------------------
# DONE:
# - Generar tabla ANOVA de la regresión.
tabla_anova <- anova(modelo_rls)
print(tabla_anova)

# - Formular H0 y H1 para la significancia global.
#teniendo la tabla anova y el modelo, podemos definir la hipótesis nula y la alterna:
#------------------------------------------------------------------------------------------
#Ho: La longitud de la aleta no presenta una relación lineal estadísticamente significativa
#con la masa corporal media de los pingüinos de la población estudiada. (Ho: B_1 = 0)
#------------------------------------------------------------------------------------------
#H1: La longitud de la aleta presenta una relación lineal estadísticamente significativa
#con la masa corporal media de los pingüinos de la población estudiada.
#------------------------------------------------------------------------------------------

# - Reportar estadístico F y valor-p.
valor_F_anova <- tabla_anova$`F value`[1]
valor_p_significancia_global <- tabla_anova$`Pr(>F)`[1]

# Nivel de significancia
alpha <- 0.05

# - Decisión sobre H0.
if (valor_p_significancia_global < alpha) {
  
  cat("Se rechaza H0: existe evidencia estadísticamente significativa de una relación 
        lineal entre la longitud de la aleta y la masa corporal media de los pingüinos.")
  
} else {
  
  cat("No se rechaza H0: no existe evidencia estadísticamente suficiente para afirmar
        que hay una relación lineal entre la longitud de la aleta y la masa corporal media de los pingüinos.")
}

# ============================================================
# 10. ESTIMACIÓN Y PREDICCIÓN
# ============================================================

# ------------------------------------------------------------
# 10.1. Valores ajustados
# ------------------------------------------------------------
# - Obtener los valores ajustados del modelo.
# - Presentar solo una cantidad razonable en el reporte/presentación.
valores_ajustados <- fitted(modelo_rls)

tabla_ajustados <- datos_modelo %>%
  mutate(Body.Mass.Ajustado..g. = valores_ajustados) %>%
  select(Flipper.Length..mm., Body.Mass..g., Body.Mass.Ajustado..g.)

cat("\n===== VALORES AJUSTADOS (primeros 10 registros) =====\n")
print(head(tabla_ajustados, 10))


# ------------------------------------------------------------
# 10.2. Selección de valores de Flipper Length
# ------------------------------------------------------------
# - Seleccionar uno o más valores pertinentes de longitud de aleta.
# - Preferir valores dentro del rango observado.
# - Justificar la selección.
rango_flipper <- range(datos_modelo$Flipper.Length..mm.)
cat("\nRango observado de Flipper Length (mm):", rango_flipper, "\n")

# Se eligen tres valores dentro del rango observado, representativos de la
# distribución de la variable: el primer cuartil, la mediana y el tercer
# cuartil. Así se cubre un pingüino "pequeño", uno "típico" y uno "grande"
# sin extrapolar fuera de los datos.
valores_flipper_seleccionados <- c(190, 200, 213)

cat("\nValores de Flipper Length seleccionados (Q1, mediana, Q3 aprox.):\n")
print(valores_flipper_seleccionados)

nuevos_datos <- data.frame(
  Flipper.Length..mm. = valores_flipper_seleccionados
)

# ------------------------------------------------------------
# 10.3. Estimación de la respuesta media
# ------------------------------------------------------------
# DONE:
# - Estimar la masa corporal media para los valores seleccionados de X.
# - Construir intervalos de confianza.
# - Interpretar los resultados.
estimacion_media <- predict(
  modelo_rls,
  newdata = nuevos_datos,
  interval = "confidence",
  level = 0.95
)

tabla_estimacion_media <- cbind(nuevos_datos, estimacion_media)

cat("\n===== ESTIMACIÓN DE LA MASA CORPORAL MEDIA (IC 95%) =====\n")
print(tabla_estimacion_media)

cat("\nInterpretación: para pingüinos con una longitud de aleta de",
    valores_flipper_seleccionados[2],
    "mm, se estima con un 95% de confianza que la masa corporal MEDIA",
    "de la población se encuentra entre",
    round(tabla_estimacion_media$lwr[2], 2), "g y",
    round(tabla_estimacion_media$upr[2], 2), "g.\n")

# ------------------------------------------------------------
# 10.4. Predicción de una respuesta individual
# ------------------------------------------------------------
# - Predecir la masa corporal de un pingüino individual.
# - Construir intervalos de predicción.
# - Comparar su amplitud con los IC de la respuesta media.
prediccion_individual <- predict(
  modelo_rls,
  newdata = nuevos_datos,
  interval = "prediction",
  level = 0.95
)

tabla_prediccion_individual <- cbind(nuevos_datos, prediccion_individual)

cat("\n===== PREDICCIÓN PARA UN PINGÜINO INDIVIDUAL (IP 95%) =====\n")
print(tabla_prediccion_individual)

cat("\nInterpretación: para UN pingüino individual con una longitud de aleta de",
    valores_flipper_seleccionados[2],
    "mm, se predice con un 95% de confianza que su masa corporal se",
    "encuentra entre",
    round(tabla_prediccion_individual$lwr[2], 2), "g y",
    round(tabla_prediccion_individual$upr[2], 2), "g.\n")

amplitud_ic <- tabla_estimacion_media$upr - tabla_estimacion_media$lwr
amplitud_ip <- tabla_prediccion_individual$upr - tabla_prediccion_individual$lwr

cat("\nComparación de amplitudes (IP siempre más ancho que el IC):\n")
print(data.frame(
  Flipper.Length..mm. = valores_flipper_seleccionados,
  Amplitud_IC_media = round(amplitud_ic, 2),
  Amplitud_IP_individual = round(amplitud_ip, 2)
))

cat("\nEsto es esperado: el IC de la media solo incorpora la incertidumbre",
    "sobre dónde está la recta de regresión, mientras que el IP de una",
    "observación individual también incorpora la variabilidad propia de",
    "un pingüino respecto a esa media, por lo que siempre es más amplio.\n")

# ------------------------------------------------------------
# 10.5. Representación gráfica
# ------------------------------------------------------------
# - Considerar observaciones, recta de regresión y bandas de intervalo.

# Secuencia de valores de X dentro del rango observado, para dibujar
# bandas de confianza y de predicción suaves sobre todo el rango.
secuencia_flipper <- data.frame(
  Flipper.Length..mm. = seq(
    rango_flipper[1], rango_flipper[2], length.out = 100
  )
)

bandas_confianza <- predict(
  modelo_rls, newdata = secuencia_flipper,
  interval = "confidence", level = 0.95
)
bandas_prediccion <- predict(
  modelo_rls, newdata = secuencia_flipper,
  interval = "prediction", level = 0.95
)

datos_bandas <- secuencia_flipper %>%
  mutate(
    ajustado = bandas_confianza[, "fit"],
    ic_inf = bandas_confianza[, "lwr"],
    ic_sup = bandas_confianza[, "upr"],
    ip_inf = bandas_prediccion[, "lwr"],
    ip_sup = bandas_prediccion[, "upr"]
  )

grafico_estimacion_prediccion <- ggplot() +
  geom_point(
    data = datos_modelo,
    aes(x = Flipper.Length..mm., y = Body.Mass..g.),
    alpha = 0.4
  ) +
  geom_ribbon(
    data = datos_bandas,
    aes(x = Flipper.Length..mm., ymin = ip_inf, ymax = ip_sup),
    fill = "steelblue", alpha = 0.15
  ) +
  geom_ribbon(
    data = datos_bandas,
    aes(x = Flipper.Length..mm., ymin = ic_inf, ymax = ic_sup),
    fill = "steelblue", alpha = 0.35
  ) +
  geom_line(
    data = datos_bandas,
    aes(x = Flipper.Length..mm., y = ajustado),
    color = "steelblue", linewidth = 1
  ) +
  geom_point(
    data = tabla_prediccion_individual,
    aes(x = Flipper.Length..mm., y = fit),
    color = "darkred", size = 2.5
  ) +
  theme_minimal() +
  labs(
    title = "Recta de regresión con bandas de confianza y de predicción",
    subtitle = "Banda oscura: IC 95% de la media | Banda clara: IP 95% individual",
    x = "Longitud de aleta (mm)",
    y = "Masa corporal (g)"
  )

print(grafico_estimacion_prediccion)

# ============================================================
# 11. DIAGNÓSTICO DEL MODELO - para el reporte y el código fuente
# ============================================================
# Nota: gran parte ya está desarrollada en las secciones 6 y 7.
# Aquí se consolidarán los resultados para el reporte final.

# ------------------------------------------------------------
# 11.1. Linealidad
# ------------------------------------------------------------
# Síntesis de la evidencia gráfica ya generada en 6.1 y 6.3.
cat("\n===== 11.1 LINEALIDAD =====\n")
cat("El diagrama de dispersión (sección 6.1) muestra una tendencia creciente",
    "y razonablemente lineal entre la longitud de la aleta y la masa corporal.",
    "El coeficiente de correlación de Pearson es de", round(coeficiente_pearson, 3),
    ", lo que respalda un supuesto de linealidad aceptable para un modelo RLS.\n")

# ------------------------------------------------------------
# 11.2. Independencia de los errores
# ------------------------------------------------------------
# Se recupera la prueba Durbin-Watson (ya calculada dentro de
# pruebas_supuestos() en la sección 6.3) y se interpreta para el reporte.

prueba_dw_reporte <- dwtest(modelo_rls)

cat("\n===== 11.2 INDEPENDENCIA DE LOS ERRORES =====\n")
print(prueba_dw_reporte)
cat("El gráfico de residuos secuenciales (sección 6.3.1) no muestra un patrón",
    "cíclico evidente. Con un valor-p de", round(prueba_dw_reporte$p.value, 4),
    if (prueba_dw_reporte$p.value >= 0.05) {
      ", no se detecta evidencia significativa de autocorrelación entre los errores.\n"
    } else {
      ", se detecta evidencia de autocorrelación entre los errores.\n"
    })

# ------------------------------------------------------------
# 11.3. Homocedasticidad
# ------------------------------------------------------------
# Se recupera la prueba Breusch-Pagan (calculada en 6.3) y se interpreta.

prueba_bp_reporte <- bptest(modelo_rls)

cat("\n===== 11.3 HOMOCEDASTICIDAD =====\n")
print(prueba_bp_reporte)
cat("Los gráficos de residuos vs. ajustados (secciones 6.3 y 6.3.1) muestran",
    "una dispersión relativamente estable a lo largo del rango de valores",
    "ajustados. Con un valor-p de", round(prueba_bp_reporte$p.value, 4),
    if (prueba_bp_reporte$p.value >= 0.05) {
      ", no se detecta evidencia de heterocedasticidad.\n"
    } else {
      ", se detecta evidencia de heterocedasticidad.\n"
    })

# ------------------------------------------------------------
# 11.4. Normalidad aproximada
# ------------------------------------------------------------
# Se recupera la prueba Shapiro-Wilk (calculada en 6.3) y se interpreta.

prueba_shapiro_reporte <- shapiro.test(residuals(modelo_rls))

cat("\n===== 11.4 NORMALIDAD APROXIMADA =====\n")
print(prueba_shapiro_reporte)
cat("El histograma de residuos y el gráfico Q-Q (secciones 6.3 y 6.3.1)",
    "muestran una forma aproximadamente simétrica, con las colas más",
    "alejadas de la línea teórica. Con un valor-p de",
    round(prueba_shapiro_reporte$p.value, 4),
    if (prueba_shapiro_reporte$p.value >= 0.05) {
      ", no se detecta una desviación significativa de la normalidad.\n"
    } else {
      ", se detecta evidencia de desviación de la normalidad.\n"
    })
# ------------------------------------------------------------
# 11.5. Residuos atípicos
# ------------------------------------------------------------
# Un residuo studentizado se considera atípico si su valor absoluto
# supera 2 (criterio habitual; algunos textos usan 3 como criterio más
# estricto). Se reportan ambos para dar contexto.

residuos_estudentizados_rls <- rstudent(modelo_rls)
n_obs_modelo <- nrow(datos_modelo)

atipicos_rls_criterio2 <- which(abs(residuos_estudentizados_rls) > 2)
atipicos_rls_criterio3 <- which(abs(residuos_estudentizados_rls) > 3)

cat("\n===== 11.5 RESIDUOS ATÍPICOS =====\n")
cat("Número de observaciones con |residuo studentizado| > 2:",
    length(atipicos_rls_criterio2), "de", n_obs_modelo, "\n")
cat("Número de observaciones con |residuo studentizado| > 3:",
    length(atipicos_rls_criterio3), "de", n_obs_modelo, "\n")

cat("\nDetalle de las observaciones con |residuo studentizado| > 2:\n")
print(
  datos_modelo[atipicos_rls_criterio2, ] %>%
    mutate(residuo_studentizado = round(
      residuos_estudentizados_rls[atipicos_rls_criterio2], 2
    ))
)
# ------------------------------------------------------------
# 11.6. Alto apalancamiento
# ------------------------------------------------------------
# Criterio de referencia habitual: leverage > 2*p/n, donde p es el número
# de parámetros del modelo (intercepto + pendiente = 2) y n el número de
# observaciones usadas para ajustar el modelo.

leverage_rls <- hatvalues(modelo_rls)
p_parametros <- length(coef(modelo_rls))
umbral_leverage <- 2 * p_parametros / n_obs_modelo

alto_leverage_rls <- which(leverage_rls > umbral_leverage)

cat("\n===== 11.6 ALTO APALANCAMIENTO =====\n")
cat("Umbral de referencia (2p/n):", round(umbral_leverage, 4), "\n")
cat("Número de observaciones con alto apalancamiento:",
    length(alto_leverage_rls), "de", n_obs_modelo, "\n")

cat("\nDetalle de las observaciones con alto apalancamiento:\n")
print(
  datos_modelo[alto_leverage_rls, ] %>%
    mutate(leverage = round(leverage_rls[alto_leverage_rls], 4))
)

# ------------------------------------------------------------
# 11.7. Observaciones influyentes
# ------------------------------------------------------------
# Criterio de referencia habitual para la distancia de Cook: > 4/n.
# No se elimina ninguna observación automáticamente; solo se identifican
# para su análisis y discusión en el reporte.

distancia_cook_rls <- cooks.distance(modelo_rls)
umbral_cook <- 4 / n_obs_modelo

influyentes_rls <- which(distancia_cook_rls > umbral_cook)

cat("\n===== 11.7 OBSERVACIONES INFLUYENTES =====\n")
cat("Umbral de referencia (4/n):", round(umbral_cook, 5), "\n")
cat("Número de observaciones potencialmente influyentes:",
    length(influyentes_rls), "de", n_obs_modelo, "\n")

cat("\nDetalle de las observaciones potencialmente influyentes",
    "(se muestran solo las 10 con mayor distancia de Cook):\n")
print(
  datos_modelo[influyentes_rls, ] %>%
    mutate(distancia_cook = round(distancia_cook_rls[influyentes_rls], 5)) %>%
    arrange(desc(distancia_cook)) %>%
    head(10)
)

plot(
  distancia_cook_rls,
  type = "h",
  main = "Distancia de Cook - RLS",
  xlab = "Índice de observación",
  ylab = "Distancia de Cook"
)
abline(h = umbral_cook, lty = 2, col = "red")

cat("\nEstas observaciones se identifican para su discusión en el reporte,",
    "pero no se eliminan del análisis: eliminar datos solo para mejorar el",
    "ajuste no está justificado sin una razón sustantiva.\n")

# ------------------------------------------------------------
# 11.8. Patrón residual por especie
# ------------------------------------------------------------
# Síntesis de lo observado en la sección 7.

cat("\n===== 11.8 PATRÓN RESIDUAL POR ESPECIE =====\n")
cat("El gráfico de residuos coloreado por especie (sección 7) muestra que",
    "los residuos no se distribuyen de forma aleatoria respecto a Species:",
    "se agrupan de forma diferenciada según la especie, lo que genera una",
    "curvatura visible en el gráfico de residuos vs. ajustados. Esto sugiere",
    "que Species captura parte de la variabilidad que el modelo RLS,",
    "al usar solo Flipper Length, no explica. Según lo comentado en clase,",
    "esto podría deberse a una variable latente no incluida en el curso;",
    "por ello, el RLS se conserva como el modelo principal del proyecto,",
    "y el posible efecto de Species se explora únicamente de forma",
    "complementaria (ver sección 16).\n")
# ============================================================
# 12. MODELO FINAL
# ============================================================

cat("\n============================================================\n")
cat("12. MODELO FINAL - SÍNTESIS PARA EL REPORTE\n")
cat("============================================================\n")

# - Presentar la ecuación estimada final.
cat("\nEcuación estimada final:\n")
cat("Masa corporal (g) =", round(intercepto_modelo_RLS, 2), "+",
    round(pendiente_modelo_RLS, 2), "* Longitud de aleta (mm)\n")

# - Sintetizar la interpretación de los coeficientes.
# - Describir dirección e intensidad de la relación.
cat("\nInterpretación de los coeficientes:\n")
cat("- Pendiente (B1 =", round(pendiente_modelo_RLS, 2), "g/mm): por cada",
    "milímetro adicional de longitud de aleta, la masa corporal media del",
    "pingüino aumenta en aproximadamente", round(pendiente_modelo_RLS, 2),
    "gramos. La relación es positiva y, dado el valor-p de la prueba F",
    "(", format(valor_p_significancia_global, scientific = TRUE, digits = 3),
    "), estadísticamente significativa.\n")
cat("- Intercepto (B0 =", round(intercepto_modelo_RLS, 2), "g): corresponde a",
    "la masa corporal esperada para una longitud de aleta de 0 mm, un valor",
    "sin sentido biológico por estar muy fuera del rango observado; se",
    "reporta solo por completitud matemática del modelo.\n")
cat("- La intensidad de la relación es alta: el coeficiente de correlación",
    "de Pearson es", round(coeficiente_pearson, 3), ".\n")

# - Resumir R^2 y capacidad explicativa.
cat("\nCapacidad explicativa del modelo:\n")
cat("- R^2 =", round(resumen_modelo_rls$r.squared, 4), "→ el modelo explica",
    "aproximadamente", round(resumen_modelo_rls$r.squared * 100, 1),
    "% de la variabilidad de la masa corporal.\n")
cat("- R^2 ajustado =", round(resumen_modelo_rls$adj.r.squared, 4), "\n")
cat("- Error estándar residual =", round(resumen_modelo_rls$sigma, 2), "g.\n")

# - Resumir estimaciones y predicciones.
cat("\nEstimaciones y predicciones (sección 10):\n")
cat("- Para una longitud de aleta de", valores_flipper_seleccionados[2],
    "mm, la masa corporal media se estima entre",
    round(tabla_estimacion_media$lwr[2], 1), "g y",
    round(tabla_estimacion_media$upr[2], 1), "g (IC 95%).\n")
cat("- Para un pingüino individual con esa misma longitud de aleta, la",
    "masa corporal predicha se ubica entre",
    round(tabla_prediccion_individual$lwr[2], 1), "g y",
    round(tabla_prediccion_individual$upr[2], 1), "g (IP 95%),",
    "un intervalo más ancho que el de la media, como es esperable.\n")

# - Sintetizar el cumplimiento razonable de supuestos.
cat("\nCumplimiento de supuestos (sección 11):\n")
cat("- Linealidad: razonablemente cumplida (r =", round(coeficiente_pearson, 3), ").\n")
cat("- Independencia: Durbin-Watson valor-p =",
    round(prueba_dw_reporte$p.value, 4),
    if (prueba_dw_reporte$p.value >= 0.05) "(sin evidencia de autocorrelación).\n" else "(evidencia de autocorrelación).\n")
cat("- Homocedasticidad: Breusch-Pagan valor-p =",
    round(prueba_bp_reporte$p.value, 4),
    if (prueba_bp_reporte$p.value >= 0.05) "(sin evidencia de heterocedasticidad).\n" else "(evidencia de heterocedasticidad).\n")
cat("- Normalidad: Shapiro-Wilk valor-p =",
    round(prueba_shapiro_reporte$p.value, 4),
    if (prueba_shapiro_reporte$p.value >= 0.05) "(sin desviación significativa de la normalidad).\n" else "(evidencia de desviación de la normalidad).\n")

# - Mencionar observaciones atípicas, leverage e influencia.
cat("\nObservaciones especiales:\n")
cat("-", length(atipicos_rls_criterio2), "observaciones con residuo",
    "studentizado atípico (|t| > 2) de", n_obs_modelo, "totales.\n")
cat("-", length(alto_leverage_rls), "observaciones con alto apalancamiento.\n")
cat("-", length(influyentes_rls), "observaciones potencialmente influyentes",
    "según la distancia de Cook. Ninguna fue eliminada del análisis.\n")

# - Señalar limitaciones y utilidad contextual del modelo.
cat("\nLimitaciones y utilidad del modelo:\n")
cat("El modelo RLS captura de forma sólida la relación lineal entre la",
    "longitud de la aleta y la masa corporal, con un ajuste alto (R^2 ≈",
    round(resumen_modelo_rls$r.squared, 2), ") y supuestos razonablemente",
    "cumplidos. Su principal limitación es que no incorpora la especie",
    "(Species), cuyo patrón residual (sección 7 y 11.8) sugiere que explica",
    "parte de la variabilidad no capturada. Por ello, el modelo es útil",
    "como herramienta descriptiva y predictiva general para la población",
    "combinada de las tres especies, pero debe usarse con cautela si se",
    "necesita precisión a nivel de una especie particular; para ese caso,",
    "los modelos ANCOVA exploratorios (sección 16) ofrecen una alternativa",
    "a considerar en trabajos futuros.\n")

# ============================================================
# 13. PRUEBA DE HIPÓTESIS PARA UNA MEDIA
# ============================================================
# TODO:
# Pregunta de interés:
# Variable:
# Población:
# Parámetro:
# Valor de referencia documentado:
# H0:
# H1:
# Nivel de significancia:
# Condiciones / supuestos:
# Estadístico de prueba:
# Valor-p:
# Intervalo de confianza:
# Decisión:
# Conclusión contextual:


# ============================================================
# 14. PRUEBA DE HIPÓTESIS PARA DIFERENCIA DE MEDIAS
# ============================================================
# TODO:
# Pregunta de interés:
# Variable cuantitativa:
# Variable categórica de agrupación:
# Grupo 1:
# Grupo 2:
# Criterio de selección de grupos:
# Parámetro:
# H0:
# H1:
# Nivel de significancia:
# Condiciones / supuestos:
# Tipo de prueba:
# Estadístico de prueba:
# Valor-p:
# Intervalo de confianza:
# Decisión:
# Conclusión contextual:


# ============================================================
# 15. PRUEBA DE BONDAD DE AJUSTE
# ============================================================
# TODO:
# Pregunta de interés:

# ¿La masa corporal de los pingüinos Chinstrap sigue una distribución normal?

# Variable cuantitativa:

# Masa corporal (Body.Mass..g.)
# medida en gramos de los pingüinos Chinstrap.

# Distribución teórica propuesta:
# Distribución normal.

# Justificación de la distribución:

#Se analiza la especie Chinstrap porque las diferentes especies 
#de pingüinos pueden presentar diferencias en su masa corporal
# Trabajar con una sola especie permite evaluar su distribución 
# de manera más homogénea y evita que la combinación de especies 
# afecte el ajuste a una distribución normal.

# Filtrar únicamente la especie Chinstrap
chinstrap <- subset(penguins, Species == "Chinstrap penguin (Pygoscelis antarctica)")

# Histograma de la masa corporal
ggplot(chinstrap, aes(x = Body.Mass..g.)) +
  geom_histogram(
    bins = 10,
    color = "black",
    fill = "lightgray"
  ) +
  labs(
    title = "Distribución de la masa corporal de los pingüinos Chinstrap",
    x = "Masa corporal (g)",
    y = "Frecuencia"
  ) +
  theme_minimal()

ggplot(chinstrap, aes(sample = Body.Mass..g.)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Gráfico Q-Q de la masa corporal de los pingüinos Chinstrap",
    x = "Cuantiles teóricos",
    y = "Cuantiles observados"
  ) +
  theme_minimal()

# Parámetros de la distribución:

masa <- na.omit(chinstrap$Body.Mass..g.)

media <- mean(masa)
desv <- sd(masa)
cat("Parametros")

cat("Número de observaciones:", length(masa), "\n")

cat("Media (μ):", round(media, 4), "g\n")
cat("Desviación estándar (σ):", round(desv, 4), "g\n")



# H0:

cat(
  "H0: La distribución de la masa corporal de los pingüinos Chinstrap ",
  "es compatible con una distribución normal con media ",
  round(media, 4),
  "g y desviación estándar ",
  round(desv, 4),
  "g.\n"
)

# H1:

cat(
  "H1: La distribución de la masa corporal de los pingüinos Chinstrap ",
  "no es compatible con una distribución normal con media ",
  round(media, 4),
  "g y desviación estándar ",
  round(desv, 4),
  "g.\n"
)

# Nivel de significancia:
alpha <- 0.05

cat("Nivel de significancia: α =", alpha, "\n")


#calcular rangos de masa
cat(
  "Mínimo:",
  round(min(masa), 4),
  "g\n"
)

cat(
  "Máximo:",
  round(max(masa), 4),
  "g\n"
)



# Probabilidades acumuladas para dividir la normal
prob <- seq(0, 1, length.out = 7)

# Puntos de corte de la distribución normal
cortes <- qnorm(
  prob,
  mean = media,
  sd = desv
)

# Mostrar los puntos de corte
round(cortes, 4)


intervalos <- cut(
  masa,
  breaks = cortes,
  include.lowest = TRUE
)

# Frecuencias observadas
frecuencias_observadas <- table(intervalos)

frecuencias_observadas


#frecuencias esperadas

frecuencia_esperada <- rep(
  length(masa) / 6,
  6
)

round(frecuencia_esperada, 4)


# Comprobación del estadístico χ²

tabla_chi <- data.frame(
  Intervalo = names(frecuencias_observadas),
  Observada = as.numeric(frecuencias_observadas),
  Esperada = frecuencia_esperada
)

tabla_chi$Aporte_chi <- (
  (tabla_chi$Observada - tabla_chi$Esperada)^2
) / tabla_chi$Esperada

# Mostrar la tabla con 4 decimales
tabla_mostrar <- tabla_chi

tabla_mostrar$Esperada <- round(
  tabla_mostrar$Esperada, 4
)

tabla_mostrar$Aporte_chi <- round(
  tabla_mostrar$Aporte_chi, 4
)

tabla_mostrar

# Suma de los aportes sin redondear
cat(
  "Suma de los aportes:",
  round(sum(tabla_chi$Aporte_chi), 4),
  "\n"
)



# Condiciones / supuestos
cat("Condiciones / supuestos:\n")

cat("- Los datos corresponden únicamente a pingüinos de la especie Chinstrap.\n")
cat("- Se trabaja con 6 intervalos de igual probabilidad bajo la distribución normal propuesta.\n")
cat("- Las frecuencias esperadas deben ser mayores o iguales a 5.\n")

cat("Frecuencia esperada mínima:", min(frecuencia_esperada), "\n")


# Estadístico de prueba:

chi_cuadrado <- sum(
  (frecuencias_observadas - frecuencia_esperada)^2 /
    frecuencia_esperada
)

cat("Estadístico de prueba χ²:", round(chi_cuadrado, 4), "\n")

grados_libertad <- 6 - 1 - 2

cat("Grados de libertad:", grados_libertad, "\n")


# Valor-p:

valor_p <- pchisq(
  chi_cuadrado,
  df = grados_libertad,
  lower.tail = FALSE
)

cat("Valor-p:", round(valor_p, 4), "\n")


# Decisión:


cat("Nivel de significancia (α):", alpha, "\n")
cat("Comparación:", round(valor_p, 4), ">", alpha, "\n")
cat("Decisión: No se rechaza H0.\n")


# Conclusión contextual:
cat("Conclusión: No existe evidencia estadísticamente significativa para afirmar que la masa corporal 
de los pingüinos Chinstrap no se ajusta a una distribución normal. Por lo tanto, los datos son compatibles 
con una distribución normal.\n")












# NO PRESTAR ATENCIÓN

# ============================================================
# 16. MODELOS ALTERNATIVOS / EXPLORATORIOS
# ============================================================
# Estos modelos NO sustituyen a la RLS principal.
# Se conservan solo como exploración del posible efecto de Species.

# ------------------------------------------------------------
# 16.1. ANCOVA sin interacción
# ------------------------------------------------------------

modelo_ancova <- lm(
  Body.Mass..g. ~ Flipper.Length..mm. + Species,
  data = datos_modelo
)

cat("\n===== ANCOVA SIN INTERACCIÓN =====\n")
print(summary(modelo_ancova))
pruebas_supuestos(modelo_ancova, "ANCOVA sin interacción")
graficar_residuos(modelo_ancova, "ANCOVA sin interacción")
graficar_residuos(modelo_ancova, "ANCOVA sin interacción", studentizados = TRUE)

# ------------------------------------------------------------
# 16.2. ANCOVA con interacción
# ------------------------------------------------------------

modelo_ancova_interaccion <- lm(
  Body.Mass..g. ~ Flipper.Length..mm. * Species,
  data = datos_modelo
)

cat("\n===== ANCOVA CON INTERACCIÓN =====\n")
print(summary(modelo_ancova_interaccion))
pruebas_supuestos(modelo_ancova_interaccion, "ANCOVA con interacción")
graficar_residuos(modelo_ancova_interaccion, "ANCOVA con interacción")

# ------------------------------------------------------------
# 16.3. ANCOVA con interacción y transformación logarítmica
# ------------------------------------------------------------

modelo_ancova_log <- lm(
  log(Body.Mass..g.) ~ Flipper.Length..mm. * Species,
  data = datos_modelo
)

cat("\n===== ANCOVA CON INTERACCIÓN - LOG(Y) =====\n")
print(summary(modelo_ancova_log))
pruebas_supuestos(modelo_ancova_log, "ANCOVA con interacción - log(Y)")
graficar_residuos(modelo_ancova_log, "ANCOVA con interacción - log(Y)")


# ============================================================
# 17. NOTAS PARA EL REPORTE
# ============================================================
# - Variable respuesta (Y): Body.Mass..g.
# - Variable explicativa (X): Flipper.Length..mm.
# - Variable categórica de agrupación: Species.
# - La RLS es el modelo principal del proyecto.
# - Los ANCOVA son análisis exploratorios complementarios.
# - No eliminar observaciones solo para mejorar los supuestos.
# - Justificar toda exclusión, recodificación o transformación.
# - Las pruebas complementarias deben incluir pregunta, variables/poblaciones,
#   parámetro, H0, H1, alfa, condiciones, estadístico, valor-p, decisión y
#   conclusión contextual.