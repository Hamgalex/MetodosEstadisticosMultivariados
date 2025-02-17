library(readxl)
muestra1 <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T5/datos_tarea.xlsx", sheet=1)
muestra2 <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T5/datos_tarea.xlsx", sheet=2)

pdh_dos_medias_grandes(muestra1,muestra2,0.05)

#' Hace una prueba de hipótesis para saber si la media poblacional 
#' es igual dada MUESTRA GRANDE
pdh_dos_medias_grandes<- function(muestra1, muestra2, alpha) {
  
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
ic_diferencia_medias_grandes <- function(muestra1, muestra2, alpha) {
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
    Media_Dif = xbarra1 - xbarra2,
    IC_Lower = ic_inf,
    IC_Upper = ic_sup
  )
  
  print(ic)
}

