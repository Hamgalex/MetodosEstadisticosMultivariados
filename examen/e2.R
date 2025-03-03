source("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/AUXR/Utils.R")
#A
Sigma <- matrix(c(81,22,-16,
                  22,64,9,
                  -16,9,21), nrow=3,byrow=TRUE)

n <- 42

a <- matrix(c(1,-1,1), nrow=3, byrow=TRUE)
mu <- matrix(c(15,-32,10), nrow=3, byrow=TRUE)

calcular_prob_comb_lineal_menor_a_X(a,mu,Sigma/n,54)

#B 
A <- c(59,13,19)
calcular_distancia_mahalanobis_alternativa(A,mu,Sigma/n)

#C
A <- matrix(c(2,-1,0,
              1,0,-3), nrow=2, byrow=TRUE)
calcular_distribucion_vector_lineal(A,mu,Sigma/40)
