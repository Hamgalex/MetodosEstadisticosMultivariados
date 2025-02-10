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

# Inicializar un "mapa" vacío para almacenar los resultados
IC_diff_lower <- list()  # Mapa para el límite inferior
IC_diff_upper <- list()  # Mapa para el límite superior

# Calcular los intervalos de confianza para las diferencias de medias
for(i in 1:(p-1)) {
  for(j in (i+1):p) {
    
    # Calcular la diferencia de medias entre los pares i y j
    diff <- mean_vector[i] - mean_vector[j]
    
    # Calcular el margen de error
    margin_error <- sqrt((p * (n - 1)) / (n * (n - p)) * F_critical * (cov_matrix[i, i] - 2 * cov_matrix[i, j] + cov_matrix[j, j]))
    
    # Crear un nombre para el par de componentes (usando un nombre de clave único)
    pair_name <- paste("X", i, "-", "X", j, sep = "")  # Nombre del par
    
    # Guardar los límites inferior y superior en los mapas
    IC_diff_lower[[pair_name]] <- diff - margin_error  # Límite inferior
    IC_diff_upper[[pair_name]] <- diff + margin_error  # Límite superior
  }
}

# Mostrar los intervalos de confianza para las diferencias de medias
IC_diff_lower
IC_diff_upper