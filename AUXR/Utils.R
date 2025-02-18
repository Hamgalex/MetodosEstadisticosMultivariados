
#' Calculo matriz de correlaciones
#' donde cada elemento es sigma_{ij} / sqrt(sigma_{ii}*sigma_{jj})
calcular_matriz_correlaciones <- function(Sigma){
  
  D <- sqrt(diag(Sigma))  
  matriz_correlaciones <- Sigma / (D %o% D)
  return(matriz_correlaciones)
}

#' Varianza de una combinación lineal
calcular_varianza_comb_lineal <- function(Sigma, coeficientes) {
  coeficientes <- as.vector(coeficientes) 
  return(as.numeric(t(coeficientes) %*% Sigma %*% coeficientes))  
}

#' Dado un vector aleatorio Z, se forma C como la matriz
#' de coeficientes y el Sigma para calcular C mult Sigma mult C'
#'
calcular_matriz_covarianzas_vector_aleatorio <- function(Sigma, C){
  return( (C%*%Sigma) %*% t(C))
}

#' Distancia de dos vectores
#' d^2 (AB) = (x-y)' Sigma^{-1} (x-y)
calcular_distancia_mahalanobis_alternativa <- function(x, y, Sigma) {
  diff <- x - y
  Sigma_inv <- solve(Sigma)
  distancia <- t(diff) %*% Sigma_inv %*% diff
  return(sqrt(distancia))
}

#' Varianza generalizada
calcular_varianza_generalizada <- function (Sigma) {
  return(det(Sigma))
}

#' Calcula P ( comb lineal < lim_sup) dado mu y Sigma para POBLACION
#' 
#' Si es muestra aleatoria es Sigma / n
calcular_prob_comb_lineal_menor_a_X <- function(a, mu, Sigma , lim_sup){
  atmu <- t(a) %*% mu
  atSigmaa <- (t(a) %*% Sigma) %*% a
  return(pnorm(lim_sup,mean = atmu,sd=sqrt(atSigmaa)))  
}

#' Dado W, mu y Sigma, sacar la distribución de Mu (sacar nuevo mu y sigma)
calcular_distribucion_vector_lineal <- function (A, mu, Sigma){
  print("nuevo mu:")
  print(A%*% mu)
  print("nuevo sigma:")
  print((A%*% Sigma)%*% t(A))
}

#' Prueba de ajuste por gráfica de distancia
#' mahalanobis vs chi cuadrado con p grados de libertad
prueba_de_ajuste_graficando <- function(datos){
  
  p=ncol(datos)
  
  media <- colMeans(datos)
  covarianza <- cov(datos)
  
  d2 <- mahalanobis(datos,center=media, cov=covarianza)
  valChiCuadrada <- qchisq(ppoints(length(d2)),df=p)
  
  qqplot(valChiCuadrada,d2,
         xlab="Cuantiles teóricos (chi cuad)",
         ylab= "Cuantiles muestrales (d2M)")
  
  abline(0,1,col="red")
}

#' H0: mu = mu0 vs H1: mu =/= mu0
#' 
#' Est: T^2 
#' 
#' Se rechaza H0 si T^2 > (n-1)p / (n-p) * F_{alpha,p,n-p}
pdh_mu_1_muestra_suponiendo_normalidad <- function (datos,mu0,alpha){
  vector_medias <- colMeans(datos)
  cov_muestrales <- cov(datos)
  
  n <- nrow(datos)
  p <- ncol(datos)
  
  T2 <- n * t(vector_medias-mu0) %*% solve(cov_muestrales) %*% (vector_medias-mu0) 
  F_critical <- ((n-1)*p) / (n-p) * qf(1-alpha,p,n-p)
  statistic <- ((n-p)*T2) /((n-1)*p)
  p_value <- 1-pf(statistic,p,n-p)
  
  cat("t cuadrada: ",T2,"\n")
  cat("f critico:",F_critical,"\n")
  cat("p-value:",p_value,"\n")
  
  if(T2>F_critical){
    cat("Se rechaza H0, hay suficiente evidencia para decir que las mu NO es igual a mu0")
  } else{
    cat("No se rechaza H0, hay suficiente evidencia para decir que las mu es igual a mu0")
  }
}

#' Intervalos de confianza simultaneos para mu, suponiendo normalidad multivariada.
#' 
ic_1_muestra_suponiendo_normalidad <- function(datos, alpha){
  vector_medias <- colMeans(datos)
  cov_muestrales <- cov(datos)
  
  n <- nrow(datos)
  p <- ncol(datos)
  
  f_value <- qf(1 - alpha, p, n - p)
  
  intervalos <- vector("list", length = p) 
  
  for (i in 1:p) {
    s_ii <- cov_muestrales[i, i]
    
    limite_inferior <- vector_medias[i] - sqrt((p * (n - 1)) / (n * (n - p)) * f_value * s_ii)
    limite_superior <- vector_medias[i] + sqrt((p * (n - 1)) / (n * (n - p)) * f_value * s_ii)
    
    intervalos[[i]] <- c(limite_inferior, limite_superior)
  }

  for (i in 1:p) {
    cat("Intervalo de confianza para mu_", i, ": [", intervalos[[i]][1], ", ", intervalos[[i]][2], "]\n", sep = "")
  }
}

ic_diferencia_suponiendo_normalidad <- function(datos, alpha){
  mean_vector <- colMeans(datos)
  cov_matrix <- cov(datos)
  
  n <- nrow(datos)
  p <- ncol(datos)
  
  F_critical <- qf(1 - alpha, p, n - p)
  
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
}


ic_1_muestra_bonferroni_suponiendo_normalidad <- function(datos,alpha){
  mean_vector <- colMeans(datos)
  cov_matrix <- cov(datos)
  
  n <- nrow(datos)
  p <- ncol(datos)
  
  t_critical <- qt(1 - alpha / (2 * p), df = n - 1)
  
  IC_lower <- numeric(p)
  IC_upper <- numeric(p)
  
  for (i in 1:p) {
    s_ii <- cov_matrix[i, i]
    margin_error <- t_critical * sqrt(s_ii / n) 
    
    IC_lower[i] <- mean_vector[i] - margin_error
    IC_upper[i] <- mean_vector[i] + margin_error
  }
  
  for(i in 1:p) {
    cat(IC_lower[i], " < x",i," < ",IC_upper[i],"\n")
  }
}

pdh_mu_1_muestra_grande <- function (datos,mu0,alpha){
  n <- nrow(datos) 
  p <- ncol(datos)
  
  mean_vector <- colMeans(datos)
  cov_matrix <- cov(datos)
  
  T2 <- n * t(mean_vector - mu0) %*% solve(cov_matrix) %*% (mean_vector - mu0)
  valor_critico <- qchisq(1 - alpha, df = p)
  p_value <- pchisq(T2, df = p, lower.tail = FALSE)
  cat("t cuadrada: ",T2,"\n")
  cat("f critico:",valor_critico,"\n")
  cat("p-value:",p_value,"\n")
  
  if(T2>valor_critico){
    cat("Se rechaza H0, hay suficiente evidencia para decir que las mu NO es igual a mu0")
  } else{
    cat("No se rechaza H0, hay suficiente evidencia para decir que las mu es igual a mu0")
  }
}

ic_1_muestra_grande <- function(datos, alpha){
  n <- nrow(datos) 
  p <- ncol(datos)
  
  mean_vector <- colMeans(datos)
  cov_matrix <- cov(datos)
  
  
  valor_critico <- qchisq(1 - alpha, df = p)
  IC_inf <- numeric(p)
  IC_sup <- numeric(p)
  
  
  for (i in 1:p) {
    
    s_ii <- cov_matrix[i, i]
    
    margin_error <- sqrt(valor_critico * s_ii/n)
    
    IC_inf[i] <- mean_vector[i] - margin_error
    IC_sup[i] <- mean_vector[i] + margin_error
  }
  for(i in 1:p) {
    cat(IC_inf[i], " < x",i," < ",IC_sup[i],"\n")
  }
}

ic_diferencia_muestra_grande <- function(datos, alpha){
  n <- nrow(datos) 
  p <- ncol(datos)
  
  mean_vector <- colMeans(datos)
  cov_matrix <- cov(datos)
  
  valor_critico <- qchisq(1 - alpha, df = p)
  
  IC_diff_lower <- list() 
  IC_diff_upper <- list() 
  for(i in 1:(p-1)) {
    for(j in (i+1):p) {
      
      diff <- mean_vector[i] - mean_vector[j]
      
      margin_error <- sqrt( valor_critico*(cov_matrix[i, i] - 2 * cov_matrix[i, j] + cov_matrix[j, j])/n)
      
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
}


ic_1_muestra_bonferroni_grande <- function(datos,alpha){
  n <- nrow(datos) 
  p <- ncol(datos)
  
  mean_vector <- colMeans(datos)
  cov_matrix <- cov(datos)
  
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
  
}


