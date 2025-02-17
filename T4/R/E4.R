df <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T4/datos_tarea4.xlsx", sheet=4)
df

mean_vector <- colMeans(df)
cov_matrix <- cov(df)
cov_matrix_inv <- solve(cov_matrix)

mean_vector
cov_matrix
cov_matrix_inv

n <- nrow(df) 
p <- ncol(df)
alpha <- 0.05 

mu_1 <- c(527, 53, 25)
mu_2 <- c(523, 55, 26)
mu_3 <- c(525, 53, 26)

T2_1 <- n * t(mean_vector - mu_1) %*% cov_matrix_inv %*% (mean_vector - mu_1)
T2_2 <- n * t(mean_vector - mu_2) %*% cov_matrix_inv %*% (mean_vector - mu_2)
T2_3 <- n * t(mean_vector - mu_3) %*% cov_matrix_inv %*% (mean_vector - mu_3)

T2_1
T2_2
T2_3

chi_critical <- qchisq(1 - alpha, df = p)

ic_dif_medias_chi <- function(i, j) {
  dif_medias <- mean_vector[i] - mean_vector[j]  
  
  term <- (cov_matrix[i, i] - 2 * cov_matrix[i, j] + cov_matrix[j, j]) * (chi_critical / n)
  margin <- sqrt(term)
  
  lower <- dif_medias - margin
  upper <- dif_medias + margin
  
  return(c(lower, upper))
}

for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    ic <- ic_dif_medias_chi(i, j)
    cat("Intervalo de confianza para μ_", i, " - μ_", j, ": [", round(ic[1], 4), ", ", round(ic[2], 4), "]\n")
  }
}



