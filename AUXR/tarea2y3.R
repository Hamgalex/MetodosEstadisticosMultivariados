source("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/AUXR/Utils.R")


Sigma <- matrix(c(4,3,-2,3,6,2,-2,2,5),nrow=3,byrow=TRUE)




calcular_matriz_correlaciones(Sigma)

Sigma <- matrix(c(10,-3,-1,5,-3,8,3,0,-1,3,15,1,5,0,1,4),nrow=4,byrow=TRUE)

coeficientes <- c(2,-4,3,-1)

varianza_resultado <- calcular_varianza_comb_lineal(Sigma, coeficientes)
print(varianza_resultado)

C <- matrix(c(5,-2,9,1,1,-1,3,1,-2,4,1,0),ncol=3,byrow=TRUE)
Sigma <- matrix(c(12,5,12,5,16,15,12,15,20),ncol=3,byrow=TRUE)


calcular_matriz_covarianzas_vector_aleatorio(Sigma, C)

A <- c(10,4,15)
B <- c(0,0,0)

calcular_distancia_mahalanobis_alternativa(A,B,Sigma)


Sigma <- matrix(c(10,-3,-1,5,-3,8,3,0,-1,3,15,1,5,0,1,4),ncol=4,byrow=TRUE)
calcular_varianza_generalizada(Sigma)

a <- matrix(c(3,0,4,0,-5), nrow=5, byrow=TRUE)
mu <- matrix(c(100,95,230,400,86),nrow=5, byrow=TRUE)
Sigma <- matrix(c(10,-2,1,0,3,-2,9,-3,4,5,1,-3,15,7,-2,0,4,7,20,2,3,5,-2,2,5), nrow=5, byrow = TRUE)

atmu <- t(a) %*% mu
atSigmaa <- (t(a) %*% Sigma) %*% a

1-calcular_prob_comb_lineal_menor_a_X(a,mu,Sigma,800)

A <- matrix(c(1,3,-4,6,1,2,9,-10,1,-1,0,1,0,1,-1), nrow=3, byrow=TRUE)
calcular_distribucion_vector_lineal(A,mu,Sigma)

x <- c(110,97,230,396,85)
y <- c(96,93,237,408,90)
calcular_distancia_mahalanobis_alternativa(x,y,Sigma)



a <- matrix(c(4,0,3,-1,0), nrow=5, byrow=TRUE)
mu <- matrix(c(100,95,230,400,86),nrow=5, byrow=TRUE)
Sigma <- matrix(c(10,-2,1,0,3,-2,9,-3,4,5,1,-3,15,7,-2,0,4,7,20,2,3,5,-2,2,5), nrow=5, byrow = TRUE)


calcular_prob_comb_lineal_menor_a_X(a,mu,Sigma/40,687)

x <- c(99.5,96,231,400,86.2)
calcular_distancia_mahalanobis_alternativa(x,mu,Sigma/40)

A <- matrix(c(1,0,2,-1,0,0,1,0,0,1), nrow=2, byrow=TRUE)
calcular_distribucion_vector_lineal(A,mu,Sigma/40)


library(readxl)
datos <- read_excel("C:/Users/hamga/Downloads/datos tarea 3.xlsx")
prueba_de_ajuste_graficando(datos)
