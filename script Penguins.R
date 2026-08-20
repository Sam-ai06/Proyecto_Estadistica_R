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

# ------------------------------------------------------------
# 9.1. Ecuación estimada y coeficientes
# ------------------------------------------------------------
# TODO:
# - Mostrar beta_0 estimado.
# - Mostrar beta_1 estimado.
# - Escribir la ecuación estimada de la RLS.
# - Interpretar intercepto y pendiente en contexto.

coef(modelo_rls)
summary(modelo_rls)

# ------------------------------------------------------------
# 9.2. Intervalos de confianza de los coeficientes
# ------------------------------------------------------------
# TODO:
# - Calcular IC para beta_0.
# - Calcular IC para beta_1.
# - Interpretarlos en contexto.

# ------------------------------------------------------------
# 9.3. Medidas de ajuste
# ------------------------------------------------------------
# TODO:
# - Reportar coeficiente de correlación lineal.
# - Reportar R^2.
# - Reportar R^2 ajustado si se considera pertinente.
# - Reportar error estándar residual.

# ------------------------------------------------------------
# 9.4. Significancia global de la regresión
# ------------------------------------------------------------
# TODO:
# - Generar tabla ANOVA de la regresión.
# - Formular H0 y H1 para la significancia global.
# - Reportar estadístico F y valor-p.
# - Interpretar la decisión en contexto.


# ============================================================
# 10. ESTIMACIÓN Y PREDICCIÓN
# ============================================================

# ------------------------------------------------------------
# 10.1. Valores ajustados
# ------------------------------------------------------------
# TODO:
# - Obtener los valores ajustados del modelo.
# - Presentar solo una cantidad razonable en el reporte/presentación.

# ------------------------------------------------------------
# 10.2. Selección de valores de Flipper Length
# ------------------------------------------------------------
# TODO:
# - Seleccionar uno o más valores pertinentes de longitud de aleta.
# - Preferir valores dentro del rango observado.
# - Justificar la selección.

# ------------------------------------------------------------
# 10.3. Estimación de la respuesta media
# ------------------------------------------------------------
# TODO:
# - Estimar la masa corporal media para los valores seleccionados de X.
# - Construir intervalos de confianza.
# - Interpretar los resultados.

# ------------------------------------------------------------
# 10.4. Predicción de una respuesta individual
# ------------------------------------------------------------
# TODO:
# - Predecir la masa corporal de un pingüino individual.
# - Construir intervalos de predicción.
# - Comparar su amplitud con los IC de la respuesta media.

# ------------------------------------------------------------
# 10.5. Representación gráfica
# ------------------------------------------------------------
# TODO:
# - Considerar observaciones, recta de regresión y bandas de intervalo.


# ============================================================
# 11. DIAGNÓSTICO DEL MODELO - para el reporte y el código fuente
# ============================================================
# Nota: gran parte ya está desarrollada en las secciones 6 y 7.
# Aquí se consolidarán los resultados para el reporte final.

# ------------------------------------------------------------
# 11.1. Linealidad
# ------------------------------------------------------------
# TODO: sintetizar la evidencia gráfica.

# ------------------------------------------------------------
# 11.2. Independencia de los errores
# ------------------------------------------------------------
# TODO: recuperar e interpretar Durbin-Watson y residuos secuenciales.

# ------------------------------------------------------------
# 11.3. Homocedasticidad
# ------------------------------------------------------------
# TODO: recuperar e interpretar Breusch-Pagan y gráficos residuales.

# ------------------------------------------------------------
# 11.4. Normalidad aproximada
# ------------------------------------------------------------
# TODO: recuperar e interpretar Shapiro-Wilk, Q-Q plot e histograma.

# ------------------------------------------------------------
# 11.5. Residuos atípicos
# ------------------------------------------------------------
# TODO: identificar posibles residuos studentizados atípicos.

# ------------------------------------------------------------
# 11.6. Alto apalancamiento
# ------------------------------------------------------------
# TODO:
# - Definir criterio de referencia.
# - Identificar observaciones relevantes.

# ------------------------------------------------------------
# 11.7. Observaciones influyentes
# ------------------------------------------------------------
# TODO:
# - Identificar observaciones potencialmente influyentes.
# - Analizar su efecto sin eliminarlas automáticamente.

# ------------------------------------------------------------
# 11.8. Patrón residual por especie
# ------------------------------------------------------------
# TODO:
# - Sintetizar lo observado en la sección 7.
# - Tratar Species como posible explicación de una limitación del RLS.


# ============================================================
# 12. MODELO FINAL
# ============================================================
# TODO:
# - Presentar la ecuación estimada final.
# - Sintetizar la interpretación de los coeficientes.
# - Describir dirección e intensidad de la relación.
# - Resumir R^2 y capacidad explicativa.
# - Resumir estimaciones y predicciones.
# - Sintetizar el cumplimiento razonable de supuestos.
# - Mencionar observaciones atípicas, leverage e influencia.
# - Señalar limitaciones y utilidad contextual del modelo.


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
# Variable cuantitativa:
# Distribución teórica propuesta:
# Justificación de la distribución:
# Parámetros de la distribución:
# H0:
# H1:
# Nivel de significancia:
# Condiciones / supuestos:
# Estadístico de prueba:
# Valor-p:
# Decisión:
# Conclusión contextual:


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