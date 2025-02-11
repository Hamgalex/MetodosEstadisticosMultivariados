library(readxl)

df <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T4/datos_tarea4.xlsx", sheet=3)

# Calcular las medias y la matriz de covarianzas
mean_vector <- colMeans(df)
cov_matrix <- cov(df)

# Parámetros
n <- nrow(df)
p <- ncol(df)
alpha <- 0.05  

# Valor crítico F
F_critical <- qf(1 - alpha, p, n - p)

# Función para calcular el intervalo de confianza para la diferencia de medias
ic_dif_medias <- function(i, j) {
  # Diferencia de medias
  dif_medias <- mean_vector[i] - mean_vector[j]
  
  # Término que involucra la matriz de covarianzas
  term <- (p * (n - 1)) / (n * (n - p)) * F_critical * (cov_matrix[i, i] - 2 * cov_matrix[i, j] + cov_matrix[j, j])
  
  # Margen de error
  margin <- sqrt(term)
  
  # Límites inferior y superior
  lower <- dif_medias - margin
  upper <- dif_medias + margin
  
  return(c(lower, upper))
}

# Calcular los intervalos de confianza para cada par de grupos
for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    ic <- ic_dif_medias(i, j)
    cat("Intervalo de confianza para μ_", i, " - μ_", j, ": [", round(ic[1], 4), ", ", round(ic[2], 4), "]\n")
  }
}