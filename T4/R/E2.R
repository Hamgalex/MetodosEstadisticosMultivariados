library(readxl)

data <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T4/datos_tarea4.xlsx", sheet=2)

n <- nrow(df)
p <- ncol(df)
alpha <- 0.05 

mean_vector <- colMeans(df)
cov_matrix <- cov(df)

chi_critical <- qchisq(1 - alpha, p)


ic_medias <- function(i) {
  margin <- sqrt((chi_critical * S[i, i]) / n)
  
  lower <- x_bar[i] - margin
  upper <- x_bar[i] + margin
  
  return(c(lower, upper))
}

for (i in 1:p) {
  ic <- ic_medias(i)
  cat("Intervalo de confianza para μ_", i, ": [", ic[1], ", ", ic[2], "]\n")
}


## B

n <- nrow(data)  # Número de observaciones
p <- ncol(data)  # Número de variables
alpha <- 0.05    # Nivel de significancia global (95% de confianza)

# Medias muestrales
x_bar <- colMeans(data)

# Matriz de covarianza muestral
S <- cov(data)

# Valor crítico de la distribución t de Student
t_critical <- qt(1 - alpha / (2 * p), n - 1)

# Función para calcular los intervalos de confianza
ic_bonferroni <- function(i) {
  # Verificar que el índice i esté dentro de los límites
  if (i < 1 || i > p) {
    stop("Índice fuera de los límites.")
  }
  
  # Margen de error
  margin <- t_critical * sqrt(S[i, i] / n)
  
  # Límites del intervalo
  lower <- x_bar[i] - margin
  upper <- x_bar[i] + margin
  
  # Retornar el intervalo
  return(c(lower, upper))
}

# Calcular intervalos de confianza para cada variable
for (i in 1:p) {
  ic <- ic_bonferroni(i)
  cat("Intervalo de confianza para μ_", i, ": [", ic[1], ", ", ic[2], "]\n")
}