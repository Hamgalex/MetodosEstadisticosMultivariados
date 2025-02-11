library(readxl)

df <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T4/datos_tarea4.xlsx", sheet=2)

n <- nrow(df)
p <- ncol(df)
alpha <- 0.05 

mean_vector <- colMeans(df)
cov_matrix <- cov(df)

chi_critical <- qchisq(1 - alpha, p)


ic_medias <- function(i) {
  margin <- sqrt((chi_critical * cov_matrix[i, i]) / n)
  
  lower <- mean_vector[i] - margin
  upper <- mean_vector[i] + margin
  
  return(c(lower, upper))
}

for (i in 1:p) {
  ic <- ic_medias(i)
  cat("Intervalo de confianza para μ_", i, ": [", ic[1], ", ", ic[2], "]\n")
}


## C

z_alpha <- qnorm(1 - alpha / (2 * p)) 

ic_medias_bonferroni <- function(i) {
  margin <- z_alpha * sqrt(cov_matrix[i, i] / n)
  
  lower <- mean_vector[i] - margin
  upper <- mean_vector[i] + margin
  
  return(c(lower, upper))
}

for (i in 1:p) {
  ic <- ic_medias_bonferroni(i)
  cat("Intervalo de confianza para μ_", i, ": [", round(ic[1], 4), ", ", round(ic[2], 4), "]\n")
}
