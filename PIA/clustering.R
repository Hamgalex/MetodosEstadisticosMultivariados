library(MASS)
library(stats)
library(clusterCrit)
library(mvnormtest)
library(lattice) #ciertos graficos
library(mvoutlier)
library(mclust)
library(dbscan)
library(readxl)
library(ggplot2)
library(corrplot)
library(psych)


file_path <- "C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/PIA/pacientes.xlsx"

data <- read_excel(file_path, sheet = 1)

data <- data[, -c(1, 2)]
