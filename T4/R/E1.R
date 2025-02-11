library(readxl)


df <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T4/datos_tarea4.xlsx")


########################################### A

mean_vector <- colMeans(df)
cov_matrix <- cov(df)
cov_matrix_inv <- solve(cov_matrix)

mean_vector
cov_matrix
cov_matrix_inv

mu_0 <-c(8,74,5,2,10,9,3)

n <- nrow(df)
p <- ncol(df)
alpha <- 0.05  

f_value <- qf(1 - alpha, p, n - p)

intervalos <- vector("list", length = p)  

for (i in 1:p) {
  s_ii <- cov_matrix[i, i]
  
  limite_inferior <- mean_vector[i] - sqrt((p * (n - 1)) / (n * (n - p)) * f_value * s_ii)
  limite_superior <- mean_vector[i] + sqrt((p * (n - 1)) / (n * (n - p)) * f_value * s_ii)
  
  intervalos[[i]] <- c(limite_inferior, limite_superior)
}

for (i in 1:p) {
  cat("Intervalo de confianza para mu_", i, ": [", intervalos[[i]][1], ", ", intervalos[[i]][2], "]\n", sep = "")
}


## C
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
}