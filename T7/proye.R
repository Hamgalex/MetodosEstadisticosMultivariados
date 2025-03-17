library(readxl)
library(ggplot2)
library(corrplot)
library(psych)


file_path <- "C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/PIA/pacientes.xlsx"

datos <- read_excel(file_path, sheet = 1)
datos <- datos[, -c(1, 2)]
datos

# Calcular el índice KMO
kmo_result <- KMO(datos)
print(kmo_result)

# Realizar la prueba de esfericidad de Bartlett
bartlett_result <- cortest.bartlett(datos)
print(bartlett_result)