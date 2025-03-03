library(dplyr)
source("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/AUXR/Utils.R")
datos <- read.csv("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/examen/us_companies.csv")

muestra1 <- filter(datos, Sector == "Energy")
muestra2 <- filter(datos, Sector == "Industrials")

muestra1$Sector <- NULL 
muestra2$Sector <- NULL 

muestra1$Country <- NULL 
muestra2$Country <- NULL 

muestra1$Company <- NULL 
muestra2$Company <- NULL 

n1 <- nrow(muestra1)
n2 <- nrow(muestra2)

xbarra1 <- colMeans(muestra1)
xbarra2 <- colMeans (muestra2)

pdh_mu_2_medias_suponiendo_normalidad(muestra1,muestra2,c(0,0,0,0),0.05)
















