# Proyecto de Estadística — Palmer Penguins

Este proyecto corresponde al análisis estadístico del conjunto de datos **Palmer Penguins**, desarrollado en **R** y **RStudio**.

El proyecto está organizado para facilitar su **reproducibilidad**, de manera que pueda ser ejecutado en una computadora distinta a aquella en la que fue desarrollado.

---

## Archivos principales

La carpeta del proyecto debe contener, como mínimo, los siguientes archivos:

```text
Proyecto/
│
├── Proyecto_Penguins.Rproj
├── script Penguins.R
├── 6_penguins_lter.xlsx
├── README.md
└── renv.lock
```

### Descripción

* `Proyecto_Penguins.Rproj`: archivo del proyecto de RStudio.
* `script Penguins.R`: código fuente principal del análisis estadístico.
* `6_penguins_lter.xlsx`: conjunto de datos original utilizado en el proyecto.
* `README.md`: instrucciones para abrir y reproducir el proyecto.
* `renv.lock`: registro de las versiones de los paquetes utilizados.

El archivo original `6_penguins_lter.xlsx` no debe modificarse. Las operaciones de limpieza, filtrado o transformación necesarias para el análisis se realizan desde el código en R.

---

# Requisitos

Para ejecutar el proyecto se recomienda tener instalados:

* R
* RStudio

No es necesario instalar previamente todos los paquetes utilizados por el análisis, ya que sus dependencias pueden restaurarse mediante `renv`.

---

# Apertura del proyecto

## 1. Descargar o copiar la carpeta completa

Todos los archivos del proyecto deben mantenerse dentro de la misma carpeta.

No se recomienda ejecutar únicamente el script de manera aislada, ya que este depende del dataset y de la estructura del proyecto.

---

## 2. Abrir el proyecto en RStudio

Abrir el archivo:

```text
Proyecto_Penguins.Rproj
```

RStudio establecerá automáticamente la carpeta del proyecto como referencia para la ejecución.

Por este motivo, el código no utiliza rutas absolutas como:

```r
setwd("C:/Users/...")
```

Esto permite ejecutar el proyecto independientemente del nombre de usuario o de la ubicación donde se haya guardado la carpeta.

---

# Restauración de las dependencias

El proyecto utiliza `renv` para registrar las versiones de los paquetes requeridos.

La primera vez que se ejecute el proyecto en una computadora diferente, instalar `renv` si todavía no se encuentra disponible:

```r
install.packages("renv")
```

Posteriormente ejecutar:

```r
renv::restore()
```

Este comando instalará las versiones de los paquetes registradas en:

```text
renv.lock
```

La restauración solamente es necesaria al preparar el proyecto por primera vez en una nueva computadora o instalación de R.

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

1. Abrir `Proyecto_Penguins.Rproj`.
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
* Restaurar las dependencias mediante `renv::restore()` cuando se ejecute el proyecto en una nueva computadora.
* No es necesario modificar manualmente el directorio de trabajo.

---

# Software utilizado

El análisis fue desarrollado utilizando:

* R
* RStudio
* Paquetes de R registrados mediante `renv`

La versión exacta de las dependencias utilizadas se encuentra documentada en el archivo:

```text
renv.lock
```

---

## Ejecución resumida

Para reproducir el proyecto:

```r
# 1. Abrir Proyecto_Penguins.Rproj

# 2. Instalar renv si es necesario
install.packages("renv")

# 3. Restaurar las dependencias
renv::restore()

# 4. Abrir "script Penguins.R"

# 5. Ejecutar el script completo
```

Una vez realizados estos pasos, el análisis estadístico puede reproducirse a partir del dataset original incluido en el proyecto.
