data <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T4/datos_tarea4.xlsx", sheet=3)

n <- nrow(df)
p <- ncol(df)
alpha <- 0.05  

F_critical <- qf(1 - alpha, p, n - p)

ic_dif_medias <- function(i, j) {
  
  dif_medias <- mean_vector[i] - mean_vector[j]
  
  term <- (p * (n - 1)) / (n * (n - p)) * F_critical * (cov_matrix[i, i] - 2 * cov_matrix[i, j] + cov_matrix[j, j])
  
  margin <- sqrt(term)
  
  lower <- dif_medias - margin
  upper <- dif_medias + margin
  
  return(c(lower, upper))
}

# calcular para cada par 
for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    ic <- ic_dif_medias(i, j)
    cat("Intervalo de confianza para μ_", i, " - μ_", j, ": [", ic[1], ", ", ic[2], "]\n")
  }
