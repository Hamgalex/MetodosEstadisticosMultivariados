library(dplyr)
source("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/AUXR/Utils.R")
datos <- read.csv("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/examen/water.csv")

datos1 <- filter(datos, location == "North")
datos2 <- filter(datos, location == "South")


datos1$location <- NULL 
datos2$location <- NULL 

datos1$town <- NULL 
datos2$town <- NULL 

pdh_mu_2_medias_suponiendo_normalidad(datos1 ,datos2,c(0,0),0.05)

ic_2_muestra_suponiendo_normalidad(datos1,datos2,0.05)



