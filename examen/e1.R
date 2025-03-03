library(dplyr)
library(readxl)

source("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/AUXR/Utils.R")

# 1
# A
Sigma <- matrix(c(4,1,0.5,0.3,
                  1,3,0.2,0.6,
                  0.5,0.2,2,0.4,
                  0.3,0.6,0.4,1.5), nrow=4,byrow=TRUE)
a <- matrix(c(1,2,0,-3), nrow=4, byrow=TRUE)
mu <- matrix(c(1,-2,3,0.5), nrow=4, byrow=TRUE)

#P(X1+2X-3X4 < 2)
calcular_prob_comb_lineal_menor_a_X(a,mu,Sigma,2)

# B
A <- c(1,4,2,-2)
B <- c(3,7,-3,0)

calcular_distancia_mahalanobis_alternativa(A,B,Sigma)

#C

A <- matrix(c(2,3,0,-1,
              1,1,1,-1,
              1,-1,0,0), nrow=3, byrow=TRUE)

calcular_distribucion_vector_lineal(A,mu,Sigma)
