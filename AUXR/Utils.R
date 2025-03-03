
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

pdh_mu_2_medias_suponiendo_normalidad <- function(datos1,datos2,delta0,alpha){
  
  mean_vector1 <- colMeans(datos1)
  mean_vector2 <- colMeans(datos2)
  
  n1 <- nrow(datos1) 
  n2 <- nrow(datos2) 
  p <- ncol(datos1)
  
  Spond <- ((n1-1)*cov(datos1)+((n2-1)*cov(datos2)))/(n1+n2-2)
  
  T2 <- (t(mean_vector1-mean_vector2-delta0) %*% solve((1/n1+1/n2)*Spond)) %*% (mean_vector1-mean_vector2-delta0)
  F_critical <- qf(1 - alpha, p, n1+n2 - p-1)
  c2 <- (p*(n1+n2-2))/(n1+n2-p-1)*F_critical
  
  cat("T2: ",T2,"\n")
  cat("c2: ",c2,"\n")
  
  if (T2 > c2) {
    cat("Rechazamos H0: Hay evidencia de diferencia entre los grupos.\n")
  } else {
    cat("No se rechaza H0: No hay suficiente evidencia para decir que las medias son distintas.\n")
  }
}

ic_2_muestra_suponiendo_normalidad <- function(datos1,datos2,alpha){
  mean_vector1 <- colMeans(datos1)
  mean_vector2 <- colMeans(datos2)
  
  n1 <- nrow(datos1) 
  n2 <- nrow(datos2) 
  p <- ncol(datos1)
  
  Spond <- ((n1-1)*cov(datos1)+((n2-1)*cov(datos2)))/(n1+n2-2)
  F_critical <- qf(1 - alpha, p, n1+n2 - p-1)
  c2 <- (p*(n1+n2-2))/(n1+n2-p-1)*F_critical
  c <- sqrt(c2)
  
  intervalos <- vector("list", length = p) 
  
  for (i in 1:p) {
    s_ii <- Spond[i, i]
    
    dif_medias <- mean_vector1[i]-mean_vector2[i]
    
    margin_error <- c*sqrt((1/n1+1/n2)*s_ii)
    
    limite_inferior <- dif_medias - margin_error
    limite_superior <- dif_medias + margin_error
    
    intervalos[[i]] <- c(limite_inferior, limite_superior)
  }
  
  for (i in 1:p) {
    cat("Intervalo de confianza para mu_", i, ": [", intervalos[[i]][1], ", ", intervalos[[i]][2], "]\n", sep = "")
  }
  
}


#' Hace una prueba de hipótesis para saber si la media poblacional 
#' es igual dada MUESTRA GRANDE
pdh_mu_2_medias_grande <- function(muestra1, muestra2, alpha) {
  
  p <- ncol(muestra1)
  
  n1 <- nrow(muestra1)
  n2 <- nrow(muestra2)
  
  delta0 <- rep(0, p)
  
  xbarra1 <- colMeans(muestra1)
  xbarra2 <- colMeans(muestra2)
  
  S1 <- cov(muestra1)
  S2 <- cov(muestra2)
  
  matriz_enmedio <- (S1 / n1) + (S2 / n2)
  dif_vector <- xbarra1 - xbarra2 - delta0
  
  T2 <- as.numeric(t(dif_vector) %*% solve(matriz_enmedio) %*% dif_vector)
  
  chi_critico <- qchisq(1 - alpha, df = p)
  
  cat("T2 =", T2, "\n")
  cat("Chi-cuadrado =", chi_critico, "\n")
  
  if (T2 > chi_critico) {
    cat("Rechazamos H0: Hay evidencia de diferencia entre los grupos.\n")
  } else {
    cat("No se rechaza H0: No hay suficiente evidencia para decir que las medias son distintas.\n")
  }
}

#' Encuentra los intervalos de confianza para MUESTRA GRANDE
#' checando la media de la columna 1 de la muestra 1 
#' con la media de la columna 1  de la muestra 2 y así sucesivamente
ic_2_muestra_grandes <- function(muestra1, muestra2, alpha) {
  n1 <- nrow(muestra1)  
  n2 <- nrow(muestra2)  
  p <- ncol(muestra1)  
  
  xbarra1 <- colMeans(muestra1)
  xbarra2 <- colMeans(muestra2)
  
  S1 <- cov(muestra1)
  S2 <- cov(muestra2)
  
  var1 <- diag(S1)
  var2 <- diag(S2)
  
  chi_cuadrado <- qchisq(1 - alpha, df = p)
  
  margen_error <- sqrt(chi_cuadrado * ((var1 / n1) + (var2 / n2)))
  
  ic_inf <- (xbarra1 - xbarra2) - margen_error
  ic_sup <- (xbarra1 - xbarra2) + margen_error
  
  ic <- data.frame(
    Variable = colnames(muestra1),
    IC_Lower = ic_inf,
    IC_Upper = ic_sup
  )
  
  print(ic)
}

ic_2_muestra_bonferroni <- function (datos1,datos2,alpha){
  
  mean_vector1 <- colMeans(datos1)
  mean_vector2 <- colMeans(datos2)
  
  n1 <- nrow(datos1) 
  n2 <- nrow(datos2) 
  p <- ncol(datos1)
  
  Spond <- ((n1-1)*cov(datos1)+((n2-1)*cov(datos2)))/(n1+n2-2)
  
  t_critical <- qt(1 - alpha / (2 * p), df = n1+n2 - 1)
  
  IC_lower <- numeric(p)
  IC_upper <- numeric(p)
  
  for (i in 1:p) {
    
    diff_medias <-mean_vector1[i]-mean_vector2[i] 
    
    s_ii <- Spond[i, i]
    margin_error <- t_critical * sqrt((1/n1+1/n2)*s_ii) 
    
    IC_lower[i] <- diff_medias - margin_error
    IC_upper[i] <- diff_medias+ margin_error
  }
  
  for(i in 1:p) {
    cat(IC_lower[i], " < x1",i,"- x2",i," < ",IC_upper[i],"\n")
  }
  
}


manova_tres_muestras <- function(muestra1,muestra2,muestra3,alpha){
  
  p=ncol(muestra1)
  k=3
  
  muestra1$NumMuestra <- "1"
  muestra2$NumMuestra <- "2"
  muestra3$NumMuestra <- "3"
  
  datos <- bind_rows(muestra1, muestra2, muestra3)
  
  N=nrow(datos)
  
  manova_result <- manova(cbind(x1, x2, x3, x4, x5, x6) ~ NumMuestra, data = datos)
  
  print(summary(manova_result, test = "Pillai"))
  print(summary(manova_result, test = "Wilks"))
  print(summary(manova_result, test = "Hotelling-Lawley"))
  print(summary(manova_result, test = "Roy"))
  print(summary(manova_result, test = "Wilks")$stats[1,2])
  valor_chi <- qchisq(1 - alpha, df = p * (k - 1))
  cat("valor chi:",valor_chi,"\n")
  bartlett =- (N-1-(p+k)/2)*log(summary(manova_result, test = "Wilks")$stats[1,2])
  cat("bartlett:",bartlett,"\n")
  
  if (bartlett>valor_chi) {
    cat("Rechazamos H0: Hay evidencia para decir que las medias no son iguales.\n")
  } else {
    cat("No se rechaza H0: No hay suficiente evidencia para decir que las medias son distintas.\n")
  }
}

ic_dif_medias_bonferroni_MANOVA <- function(muestra1, muestra2, muestra3, alpha = 0.05) {
  
  p = ncol(muestra1)
  k = 3
  n1 = nrow(muestra1)
  n2 = nrow(muestra2)
  n3 = nrow(muestra3)
  N <- n1 + n2 + n3
  
  W <- ((n1 - 1) * cov(muestra1) +
          (n2 - 1) * cov(muestra2) +
          (n3 - 1) * cov(muestra3)) 
  W <- W / (N - k)
  
  wii <- diag(W)
  
  xbarra1 <- colMeans(muestra1)
  xbarra2 <- colMeans(muestra2)
  xbarra3 <- colMeans(muestra3)
  
  alpha_adjusted <- alpha / (p * k * (k - 1))
  
  t_critical <- qt(1 - alpha_adjusted, df = N - k)
  
  df <- data.frame(x_i = numeric(0), lower = numeric(0), upper = numeric(0))
  
  print("muestra 1 vs muestra 2")
  for (i in 1:p) {
    dif_medias <- xbarra1[i] - xbarra2[i]
    
    margen_error <- t_critical * sqrt((1/n1 + 1/n2) * wii[i])
    
    lim_inf <- dif_medias - margen_error
    lim_sup <- dif_medias + margen_error
    
    df <- rbind(df, data.frame(x_i = i, lower = lim_inf, upper = lim_sup))
  }
  print(df)
  
  
  df <- data.frame(x_i = numeric(0), lower = numeric(0), upper = numeric(0))
  print("muestra 2 vs muestra 3")
  for (i in 1:p) {
    dif_medias <- xbarra2[i] - xbarra3[i]
    
    margen_error <- t_critical * sqrt((1/n2 + 1/n3) * wii[i])
    
    lim_inf <- dif_medias - margen_error
    lim_sup <- dif_medias + margen_error
    
    df <- rbind(df, data.frame(x_i = i, lower = lim_inf, upper = lim_sup))
  }
  print(df)
  
  
  df <- data.frame(x_i = numeric(0), lower = numeric(0), upper = numeric(0))
  print("muestra 1 vs muestra 3")
  for (i in 1:p) {
    dif_medias <- xbarra1[i] - xbarra3[i]
    
    margen_error <- t_critical * sqrt((1/n1 + 1/n3) * (wii[i]))
    
    lim_inf <- dif_medias - margen_error
    lim_sup <- dif_medias + margen_error
    
    df <- rbind(df, data.frame(x_i = i, lower = lim_inf, upper = lim_sup))
  }
  
  print(df)
}

pdh_m_de_box <- function (muestra1,muestra2,muestra3,alpha){
  p = ncol(muestra1)
  k = 3
  n1 = nrow(muestra1)
  n2 = nrow(muestra2)
  n3 = nrow(muestra3)
  S1 = cov(muestra1)
  S2 = cov(muestra2)
  S3 = cov(muestra3)
  N <- n1 + n2 + n3
  
  Spond = ((n1-1)*S1 + (n2-1)*S2 + (n3-1)*S3) / (N-k)
  
  Lambda = (det(S1)/det(Spond))^((n1-1)/2)*(det(S2)/det(Spond))^((n2-1)/2)*(det(S3)/det(Spond))^((n3-1)/2)
  M = -2 * log(Lambda)
  
  f1 = (k-1) * p * (p+1) / 2
  
  sumap = (1/(n1-1))+(1/(n2-1))+(1/(n3-1))
  pcur = 1 - (2*p^2 +3*p -1)/(6*(p+1)*(k-1)) *(sumap-1/(N-k))
  
  sumat =  (1/(n1-1)^2)+(1/(n2-1)^2)+(1/(n3-1)^2)
  tcur = (p-1)*(p+2)/(6*(k-1)) * (sumat -1/(N-k)^2 )
  
  f2 = (f1+2)/abs(tcur-(1-pcur)^2)
  
  gamma = (pcur -f1/f2) /f1
  
  est_prueba <- gamma*M
  valor_critico <- qf(1-alpha,f1,f2)
  
  pvalor <- pf(est_prueba,f1,f2,lower.tail=FALSE)
  
  cat("f1 =", f1, "\n")
  cat("pcur =", pcur, "\n")
  cat("tcur =", tcur, "\n")
  cat("f2 =", f2, "\n")
  cat("gamma =", gamma, "\n")
  cat("est_prueba =", est_prueba, "\n")
  cat("valor_critico =", valor_critico, "\n")
  cat("pvalor =", pvalor, "\n")
  
  if(est_prueba>valor_critico){
    cat("Se rechaza H0, hay suficiente evidencia para decir que las matrices de covarianza no son iguales")
  } else{
    cat("No se rechaza H0, hay suficiente evidencia para decir que las matrices de covarianza son iguales")
  }
  
}




