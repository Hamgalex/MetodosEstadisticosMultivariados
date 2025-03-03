library(readxl)
source("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/AUXR/Utils.R")

datos <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T4/datos_tarea4.xlsx")

pdh_mu_1_muestra_suponiendo_normalidad(datos,c(8,74,5,2,10,9,3),0.05)

ic_1_muestra_suponiendo_normalidad(datos,0.05)

ic_diferencia_suponiendo_normalidad(datos,0.05)

ic_1_muestra_bonferroni_suponiendo_normalidad(datos,0.05)



############################################################

source("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/AUXR/Utils.R")

datos <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T4/datos_tarea4.xlsx", sheet=2)

mu_1 <- c(3.60, 2.00, 2.10, 2.15, 2.60, 1.30)
mu_2 <- c(3.60, 1.90, 2.10, 2.15, 2.60, 1.30)
mu_3 <- c(3.60, 2.00, 2.10, 2.15, 3.00, 1.30)


pdh_mu_1_muestra_grande(datos,mu_1,0.05)
pdh_mu_1_muestra_grande(datos,mu_2,0.05)
pdh_mu_1_muestra_grande(datos,mu_3,0.05)

ic_1_muestra_grande(datos,0.05)

ic_diferencia_muestra_grande(datos,0.05)

ic_1_muestra_bonferroni_grande(datos,0.05)


###############################################################

datos <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T4/datos_tarea4.xlsx", sheet=3)
mu0 <- c(0.85,0.79,1.8,1.7,0.7,0.7)
pdh_mu_1_muestra_suponiendo_normalidad(datos,mu0,0.05)


ic_diferencia_suponiendo_normalidad(datos,0.05)

prueba_de_ajuste_graficando(datos)

#########################################################
datos <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T4/datos_tarea4.xlsx", sheet=4)
datos
nrow(datos)

mu_1 <- c(527,53,25)
mu_2 <- c(523,55,26)
mu_3 <- c(525,53,26)


pdh_mu_1_muestra_grande(datos,mu_1,0.05)
pdh_mu_1_muestra_grande(datos,mu_2,0.05)
pdh_mu_1_muestra_grande(datos,mu_3,0.05)

ic_diferencia_muestra_grande(datos,0.05)

#########################



datos1 <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T5/datos_tarea.xlsx", sheet=1)
datos2 <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T5/datos_tarea.xlsx", sheet=2)

pdh_mu_2_medias_suponiendo_normalidad(datos1,datos2,c(0,0,0,0),0.05)
ic_2_muestra_suponiendo_normalidad(datos1,datos2,0.05)

pdh_mu_2_medias_grande(datos1,datos2,0.05)

ic_2_muestra_grandes(datos1,datos2,0.05)

ic_2_muestra_bonferroni(datos1,datos2,0.05)

######################################

library(dplyr)
source("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/AUXR/Utils.R")


# Cargar las muestras
muestra1 <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T5/datos_tarea.xlsx", sheet=4)
muestra2 <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T5/datos_tarea.xlsx", sheet=5)
muestra3 <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T5/datos_tarea.xlsx", sheet=6)


manova_tres_muestras(muestra1,muestra2,muestra3,0.05)
ic_dif_medias_bonferroni_MANOVA(muestra1,muestra2,muestra3,0.05)
pdh_m_de_box(muestra1,muestra2,muestra3,0.05)


###########
source("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/AUXR/Utils.R")
datos <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T5/datos_tarea.xlsx", sheet=7)



muestra1 <- filter(datos, universidad== "A")
muestra2 <- filter(datos, universidad == "B")
muestra3 <- filter(datos, universidad == "C")
muestra1$universidad <- NULL 
muestra2$universidad <- NULL 
muestra3$universidad <- NULL 

manova_tres_muestras(muestra1,muestra2,muestra3,0.05)
ic_dif_medias_bonferroni_MANOVA(muestra1,muestra2,muestra3,0.05)
pdh_m_de_box(muestra1,muestra2,muestra3,0.05)