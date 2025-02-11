
df <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T4/datos_tarea4.xlsx", sheet=2)
data <- df
# Medias muestrales
x_bar <- colMeans(data)

# Matriz de covarianza muestral
S <- cov(data)

# Valor crítico de la distribución chi-cuadrado
chi_critical <- qchisq(1 - alpha, p)

# Función para calcular los intervalos de confianza
ic_medias <- function(i) {
  # Margen de error
  margin <- sqrt((chi_critical * S[i, i]) / n)
  
  # Límites del intervalo
  lower <- x_bar[i] - margin
  upper <- x_bar[i] + margin
  
  # Retornar el intervalo
  return(c(lower, upper))
}

# Calcular intervalos de confianza para cada variable
for (i in 1:p) {
  ic <- ic_medias(i)
  cat("Intervalo de confianza para μ_", i, ": [", ic[1], ", ", ic[2], "]\n")
}