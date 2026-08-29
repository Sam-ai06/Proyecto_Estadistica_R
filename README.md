# Proyecto de Estadística — Palmer Penguins

Este proyecto corresponde al análisis estadístico del conjunto de datos **Palmer Penguins**, desarrollado en **R** y **RStudio**.

El proyecto está organizado para facilitar su **reproducibilidad**, de manera que pueda ser ejecutado en una computadora distinta a aquella en la que fue desarrollado.

---

## Archivos principales

La carpeta del proyecto debe contener, como mínimo, los siguientes archivos:

```text
Proyecto/
│
├── Proyecto_Estadistica_R.Rproj
├── script Penguins.R
├── 6_penguins_lter.xlsx
└─── README.md
```

### Descripción

* `Proyecto_Estadistica_R.Rproj`: archivo del proyecto de RStudio.
* `script Penguins.R`: código fuente principal del análisis estadístico.
* `6_penguins_lter.xlsx`: conjunto de datos original utilizado en el proyecto.
* `README.md`: instrucciones para abrir y reproducir el proyecto.

El archivo original `6_penguins_lter.xlsx` no debe modificarse. Las operaciones de limpieza, filtrado o transformación necesarias para el análisis se realizan desde el código en R.

---

# Requisitos

Para ejecutar el proyecto se recomienda tener instalados:

* R
* RStudio

No es necesario instalar previamente todos los paquetes utilizados por el análisis, ya que sus dependencias pueden restaurarse mediante la función integrada al inicio del script.

---

# Apertura del proyecto

## 1. Descargar o copiar la carpeta completa

Todos los archivos del proyecto deben mantenerse dentro de la misma carpeta.

No se recomienda ejecutar únicamente el script de manera aislada, ya que este depende del dataset y de la estructura del proyecto.

---

## 2. Abrir el proyecto en RStudio

Abrir el archivo:

```text
Proyecto_Estadistica_R.Rproj
```

RStudio establecerá automáticamente la carpeta del proyecto como referencia para la ejecución.

Por este motivo, el código no utiliza rutas absolutas como:

```r
setwd("C:/Users/...")
```

Esto permite ejecutar el proyecto independientemente del nombre de usuario o de la ubicación donde se haya guardado la carpeta.

---

# Ejecución del análisis

Después de abrir el archivo `.Rproj` y restaurar las dependencias, abrir:

```text
script Penguins.R
```

El script debe ejecutarse **desde el inicio hasta el final y en el orden establecido**.

En RStudio puede utilizarse:

```text
Source
```

o ejecutar todo el contenido del script.

El código carga el dataset mediante una ruta relativa al proyecto, por lo que el archivo:

```text
6_penguins_lter.xlsx
```

debe permanecer dentro de la ubicación esperada en la carpeta del proyecto.

---

# Reproducibilidad

El proyecto fue preparado para que el análisis pueda ejecutarse desde una sesión limpia de R.

Para verificar la reproducibilidad puede utilizarse el siguiente procedimiento:

1. Abrir `Proyecto_Estadistica_R.Rproj`.
2. Restaurar las dependencias con `renv::restore()` si es necesario.
3. Reiniciar la sesión de R mediante:

```text
Session → Restart R
```

4. Confirmar que el entorno de R se encuentre vacío.
5. Abrir `script Penguins.R`.
6. Ejecutar el script completo utilizando `Source`.

El análisis no depende de objetos almacenados previamente en el entorno de R.

Todas las variables, subconjuntos de datos, modelos, pruebas estadísticas y gráficos necesarios son generados nuevamente durante la ejecución del código.

---

# Dataset

El proyecto utiliza el archivo:

```text
6_penguins_lter.xlsx
```

correspondiente al conjunto de datos **Palmer Penguins**.

El dataset contiene observaciones de pingüinos y distintas características biológicas utilizadas para el análisis descriptivo, la Regresión Lineal Simple y las pruebas de hipótesis desarrolladas en el proyecto.

El archivo original se conserva sin modificaciones.

Cuando es necesario excluir observaciones con valores faltantes o crear subconjuntos para determinados procedimientos estadísticos, estas operaciones se realizan directamente desde el código, permitiendo mantener la trazabilidad con respecto al dataset original.

---

# Consideraciones

Para garantizar una ejecución correcta:

* No modificar los nombres de los archivos.
* No mover el dataset fuera de la estructura del proyecto.
* Abrir primero el archivo `.Rproj`.
* Ejecutar el script desde una sesión limpia de R.
* No es necesario modificar manualmente el directorio de trabajo.

---

# Software utilizado

El análisis fue desarrollado utilizando:

* R
* RStudio


---

## Ejecución resumida

Para reproducir el proyecto:

```r
# 1. Abrir Proyecto_Estadistica_R.Rproj

# 2. Abrir "script Penguins.R"

# 3. Ejecutar el script completo
```

Una vez realizados estos pasos, el análisis estadístico puede reproducirse a partir del dataset original incluido en el proyecto.
