library(dplyr)
source("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/AUXR/Utils.R")
datos <- read.csv("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/examen/olive.csv")

datos1 <- filter(datos, Region == "1")
datos2 <- filter(datos, Region == "2")
datos3 <- filter(datos, Region == "3")


datos1$Region <- NULL 
datos2$Region <- NULL 
datos3$Region <- NULL 

datos1$Area <- NULL 
datos2$Area <- NULL 
datos3$Area <- NULL 

datos1$X <- NULL 
datos2$X <- NULL 
datos3$X <- NULL 

colnames(datos1) <- paste0("x", 1:8)
colnames(datos2) <- paste0("x", 1:8)
colnames(datos3) <- paste0("x", 1:8)

manova_tres_muestras(datos1,datos2,datos3,0.05)

pdh_mu_2_medias_grande(datos1, datos2,0.05)
pdh_mu_2_medias_grande(datos2, datos3,0.05)
pdh_mu_2_medias_grande(datos1, datos3,0.05)




pdh_mu_2_medias_suponiendo_normalidad(datos1,datos2,c(0,0,0,0,0,0,0,0),0.05)
pdh_mu_2_medias_suponiendo_normalidad(datos2,datos3,c(0,0,0,0,0,0,0,0),0.05)
pdh_mu_2_medias_suponiendo_normalidad(datos1,datos3,c(0,0,0,0,0,0,0,0),0.05)


















