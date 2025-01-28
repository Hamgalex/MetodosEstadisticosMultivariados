C <- matrix(c(5, -2, 9, 1, 1, -1, 3, 1, -2, 4, 1, 0), nrow = 4, byrow = TRUE)
SIGMA <- matrix(c(12, 5, 12, 5, 16, 15, 12, 15, 20), nrow = 3, byrow = TRUE)
CT <- matrix(c(5, 1, 3, 4, -2, 1, 1, 1, 9, -1, -2, 0), nrow = 3, byrow = TRUE)

resultado_CSIGMA <- C %*% SIGMA
resultado <- resultado_CSIGMA %*% CT

cat("Resultado de A * B:\n")
print(resultado_CSIGMA)

cat("\nResultado de (A * B) * C:\n")
print(resultado)


SIGMA <- matrix(c(12, 5, 12, 5, 16, 15, 12, 15, 20), nrow = 3, byrow = TRUE)
SIGMAINVERSA <- solve(SIGMA)

XMENOSYTRANSPUESTA <- matrix(c(10,4,15), nrow = 1)  
SIGMA <- matrix(c(12, 5, 12, 5, 16, 15, 12, 15, 20), nrow = 3, byrow = TRUE)
SIGMAINVERSA <- solve(SIGMA)
XMENOSY <- matrix(c(10,4,15), nrow = 3)

RESULTADO1 <- XMENOSYTRANSPUESTA %*% SIGMAINVERSA
RESULTADO <- RESULTADO1 %*% XMENOSY

print(RESULTADO1)

print(RESULTADO)



v <- matrix(c(2, -7, 7), nrow = 1)  # Vector fila
M <- matrix(c(0.6985, 0.5882, -0.8602,
              0.5882, 0.7058, -0.8823,
              -0.8602, -0.8823, 1.2279), nrow = 3, byrow = TRUE)  # Matriz 3x3
v_t <- matrix(c(2, -7, 7), nrow = 3)  # Vector columna

# Realizar la operación: v * M * v_t
result <- v %*% M %*% v_t

# Imprimir los resultados
cat("Vector fila (v):\n")
print(v)

cat("\nMatriz (M):\n")
print(M)

cat("\nVector columna (v_t):\n")
print(v_t)

cat("\nResultado de v * M * v_t:\n")
print(result)



# Definir la matriz Sigma
Sigma <- matrix(c(10, -3, -1, 5,
                  -3,  8,  3, 0,
                  -1,  3, 15, 1,
                  5,  0,  1, 4), 
                nrow = 4, byrow = TRUE)
det_Sigma <- det(Sigma)

qchisq(0.85, 5)


install.packages("readxl")

# Cargar el paquete
library(readxl)


data <- read_excel("C:/Users/hamga/Downloads/datos_tarea2.xlsx")
media_muestral <- colMeans(data)
matriz_covarianzas <- cov(data) 

print(matriz_covarianzas)

# Imprimir los resultados
cat("Medias muestrales:\n", media_muestral, "\n")
cat("Desviaciones estándar muestrales:\n", desviacion_muestral, "\n")


