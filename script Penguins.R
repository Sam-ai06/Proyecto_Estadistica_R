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

#dimensiones del dataset
dim(penguins)

#nombres de las columnas
names(penguins)

#valores de body mass
penguins[["Body.Mass..g."]]

#valores nulos de body mass
sum(is.na(penguins[["Body.Mass..g."]])) #342 de 344 registros completos 


