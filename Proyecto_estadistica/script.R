#librería para cargar datos a partir de un csv
library(readr)

#librerías para exploración y limpieza de datos
library(dplyr)
library(tidyr)
library(ggplot2)

penguin_data <- read_excel("6_penguins_lter.xlsx")
str(penguins)
head(penguins)

getwd()
