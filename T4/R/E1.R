library(readxl)


df <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T4/datos_tarea4.xlsx")


# 1
# A

mean_vector <- colMeans(df)
cov_matrix <- cov(df)
cov_matrix_inv <- solve(cov_matrix)

mean_vector
cov_matrix
cov_matrix_inv

mu_0 <-c(8,74,5,2,10,9,3)

n <- nrow(df)
p <- ncol(df)
alpha <- 0.05  # Nivel de significancia

# Calcular el valor crítico F de la distribución F
f_value <- qf(1 - alpha, p, n - p)

# Calcular los límites de los intervalos de confianza para cada componente del vector de medias
intervalos <- vector("list", length = p)  # Crear una lista para almacenar los intervalos

for (i in 1:p) {
  # Elemento i-ésimo de la matriz de varianzas-covarianzas (s_ii)
  s_ii <- cov_matrix[i, i]
  
  # Calcular los límites inferior y superior para el intervalo de confianza de la i-ésima media
  limite_inferior <- mean_vector[i] - sqrt((p * (n - 1)) / (n * (n - p)) * f_value * s_ii)
  limite_superior <- mean_vector[i] + sqrt((p * (n - 1)) / (n * (n - p)) * f_value * s_ii)
  
  # Almacenar los intervalos en la lista
  intervalos[[i]] <- c(limite_inferior, limite_superior)
}

# Imprimir los intervalos
for (i in 1:p) {
  cat("Intervalo de confianza para mu_", i, ": [", intervalos[[i]][1], ", ", intervalos[[i]][2], "]\n", sep = "")
}