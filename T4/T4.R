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

T2 <- nrow(df) * t(mean_vector - mu_0) %*% cov_matrix_inv %*% (mean_vector - mu_0)
T2

n <- nrow(df) 
p <- ncol(df)
alpha <- 0.05 

F_critical <- ((n - 1) * p) / (n - p) * qf(1 - alpha, p, n - p)
F_critical

statistic <- ((n - p) * T2) / ((n - 1) * p)
p_value <- 1 - pf(statistic, p, n - p)
p_value

# B

IC_inf <- numeric(p)  
IC_sup <- numeric(p)  

for (i in 1:p) {
  # Obtener el valor de s_ii
  s_ii <- cov_matrix[i, i]
  
  # Calcular el margen de error para el intervalo de confianza
  margin_error <- sqrt((p * (n - 1)) / (n * (n - p)) * F_critical * s_ii)
  
  # Calcular el intervalo de confianza para el componente i
  IC_lower[i] <- mean_vector[i] - margin_error
  IC_upper[i] <- mean_vector[i] + margin_error
}

IC_lower
IC_upper


# C

IC_diff_lower <- list() 
IC_diff_upper <- list()  

for(i in 1:(p-1)) {
  for(j in (i+1):p) {
  
    diff <- mean_vector[i] - mean_vector[j]
    
    margin_error <- sqrt((p * (n - 1)) / (n * (n - p)) * F_critical * (cov_matrix[i, i] - 2 * cov_matrix[i, j] + cov_matrix[j, j]))
  
    pair_name <- paste("X", i, "-", "X", j, sep = "") 

    IC_diff_lower[[pair_name]] <- diff - margin_error  
    IC_diff_upper[[pair_name]] <- diff + margin_error  
  }
}

for(pair in names(IC_diff_lower)) {
  cat("Intervalo de Confianza para la diferencia de medias entre ", pair, ":\n", sep="")
  cat("Límite Inferior: ", IC_diff_lower[[pair]], "\n")
  cat("Límite Superior: ", IC_diff_upper[[pair]], "\n\n")
}

# D

t_critical <- qt(1 - alpha / (2 * p), df = n - 1)

IC_lower <- numeric(p)
IC_upper <- numeric(p)

for (i in 1:p) {
  s_ii <- cov_matrix[i, i]
  margin_error <- t_critical * sqrt(s_ii / n) 
  
  IC_lower[i] <- mean_vector[i] - margin_error
  IC_upper[i] <- mean_vector[i] + margin_error
}

IC_lower
IC_upper


## 2


df <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T4/datos_tarea4.xlsx", sheet=2)

n <- nrow(df) 
p <- ncol(df)
alpha <- 0.05 

mean_vector <- colMeans(df)
cov_matrix <- cov(df)
cov_matrix_inv <- solve(cov_matrix)

mean_vector
cov_matrix
cov_matrix_inv


mu_1 <- c(3.60, 2.00, 2.10, 2.15, 2.60, 1.30)
mu_2 <- c(3.60, 1.90, 2.10, 2.15, 2.60, 1.30)
mu_3 <- c(3.60, 2.00, 2.10, 2.15, 3.00, 1.30)


T2_1 <- n * t(mean_vector - mu_1) %*% cov_matrix_inv %*% (mean_vector - mu_1)
T2_2 <- n * t(mean_vector - mu_2) %*% cov_matrix_inv %*% (mean_vector - mu_2)
T2_3 <- n * t(mean_vector - mu_3) %*% cov_matrix_inv %*% (mean_vector - mu_3)

T2_1
T2_2
T2_3

valor_critico <- qchisq(1 - alpha, df = p)
valor_critico



# B
IC_inf <- numeric(p)
IC_sup <- numeric(p)


for (i in 1:p) {
  
  s_ii <- cov_matrix[i, i]
  
  margin_error <- sqrt((p * (n - 1)) / (n * (n - p)) * valor_critico * s_ii)
  
  IC_inf[i] <- mean_vector[i] - margin_error
  IC_sup[i] <- mean_vector[i] + margin_error
}
IC_inf
IC_sup

# C

t_critical <- qt(1 - alpha / (2 * p), df = n - 1)

IC_inf <- numeric(p)
IC_sup <- numeric(p)

for (i in 1:p) {
  s_ii <- cov_matrix[i, i]  # Obtener la varianza de la componente i
  margin_error <- t_critical * sqrt(s_ii / n)  # Calcular el margen de error
  
  IC_inf[i] <- mean_vector[i] - margin_error  # Asignar límite inferior
  IC_sup[i] <- mean_vector[i] + margin_error  # Asignar límite superior
}

IC_inf
IC_sup


# 3

df <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T4/datos_tarea4.xlsx", sheet=3)
df

mean_vector <- colMeans(df)
cov_matrix <- cov(df)
cov_matrix_inv <- solve(cov_matrix)

mean_vector
cov_matrix
cov_matrix_inv

mu_0 <- c(0.85, 0.79, 1.80, 1.70, 0.7, 0.7)

T2 <- nrow(df) * t(mean_vector - mu_0) %*% cov_matrix_inv %*% (mean_vector - mu_0)
T2

n <- nrow(df) 
p <- ncol(df)
alpha <- 0.05 

F_critical <- ((n - 1) * p) / (n - p) * qf(1 - alpha, p, n - p)
F_critical

statistic <- ((n - p) * T2) / ((n - 1) * p)
statistic

# B

IC_diff_lower <- list() 
IC_diff_upper <- list()  

for(i in 1:(p-1)) {
  for(j in (i+1):p) {
    
    diff <- mean_vector[i] - mean_vector[j]
    
    margin_error <- sqrt((p * (n - 1)) / (n * (n - p)) * F_critical * (cov_matrix[i, i] - 2 * cov_matrix[i, j] + cov_matrix[j, j]))
    
    pair_name <- paste("X", i, "-", "X", j, sep = "") 
    
    IC_diff_lower[[pair_name]] <- diff - margin_error  
    IC_diff_upper[[pair_name]] <- diff + margin_error  
  }
}

for(pair in names(IC_diff_lower)) {
  cat("Intervalo de Confianza para la diferencia de medias entre ", pair, ":\n", sep="")
  cat("Límite Inferior: ", IC_diff_lower[[pair]], "\n")
  cat("Límite Superior: ", IC_diff_upper[[pair]], "\n\n")
}


# C

mean_vector <- colMeans(df)
cov_matrix <- cov(df)

d2 <- mahalanobis(df, center = mean_vector, cov = cov_matrix)
valChiCuadrada <- qchisq(ppoints(length(d2)), df = 6)

qqplot(valChiCuadrada, d2,
       xlab = "Cuantiles teóricos(chi-cuad)",
       ylab = "cuantiles muestrales (d2M)")

abline(0, 1, col = "red")  

# 4

# A

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

F_critical <- ((n - 1) * p) / (n - p) * qf(1 - alpha, p, n - p)
F_critical

statistic <- ((n - p) * T2) / ((n - 1) * p)
statistic

# B 
IC_diff_lower <- list() 
IC_diff_upper <- list()  

for(i in 1:(p-1)) {
  for(j in (i+1):p) {
    
    diff <- mean_vector[i] - mean_vector[j]
    
    margin_error <- sqrt((p * (n - 1)) / (n * (n - p)) * F_critical * (cov_matrix[i, i] - 2 * cov_matrix[i, j] + cov_matrix[j, j]))
    
    pair_name <- paste("X", i, "-", "X", j, sep = "") 
    
    IC_diff_lower[[pair_name]] <- diff - margin_error  
    IC_diff_upper[[pair_name]] <- diff + margin_error  
  }
}

for(pair in names(IC_diff_lower)) {
  cat("Intervalo de Confianza para la diferencia de medias entre ", pair, ":\n", sep="")
  cat("Límite Inferior: ", IC_diff_lower[[pair]], "\n")
  cat("Límite Superior: ", IC_diff_upper[[pair]], "\n\n")
}

