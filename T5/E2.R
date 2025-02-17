

n1 <- 60
n2 <- 80
p <- 7

xbarra1 <- c(124.030, 133.900, 8.961, 7.988, 51.300, 39.730, 8.917)
xbarra2 <- c(125.730, 137.920, 8.206, 8.367, 50.930, 40.726, 9.179)

S1 <- matrix(c(599.92092, 645.84289, 45.50137, 39.21181, 227.3114, 188.15965, 43.29901,
                   645.84289, 696.7868, 48.46133, 41.69736, 244.34511, 202.89604, 46.87387,
                   45.50137, 48.46133, 4.8676, 4.04492, 17.47205, 14.16381, 2.87871,
                   39.21181, 41.69736, 4.04492, 4.11783, 14.73732, 11.3687, 2.59717,
                   227.3114, 244.34511, 17.47205, 14.73732, 97.0569, 70.60495, 16.07998,
                   188.15965, 202.89604, 14.16381, 11.3687, 70.60495, 63.48021, 13.68793,
                   43.29901, 46.87387, 2.87871, 2.59717, 16.07998, 13.68793, 3.31663), 
                 nrow = 7, byrow = TRUE)

S2 <- matrix(c(575.27043, 618.91886, 45.70737, 38.20195, 225.8663, 175.25235, 41.49424,
                    618.91886, 667.58465, 48.858, 40.40396, 243.20193, 189.21062, 44.853,
                    45.70737, 48.858, 4.71999, 3.95054, 18.03344, 13.29122, 3.01997,
                    38.20195, 40.40396, 3.95054, 4.17723, 14.44286, 10.82841, 2.56922,
                    225.8663, 243.20193, 18.03344, 14.44286, 95.49805, 67.83094, 16.08471,
                    175.25235, 189.21062, 13.29122, 10.82841, 67.83094, 58.56777, 12.80255,
                    41.49424, 44.853, 3.01997, 2.56922, 16.08471, 12.80255, 3.14181),
                  nrow = 7, byrow = TRUE)

pdh_dos_medias_grandes_dados_mu_y_S(n1,n2,p,xbarra1,xbarra2,s1,s2,0.05)

#'prubea de hipotesis dos medias grandes
#'No tenemos los valores de las muestras
#'Pero tenemos los vectores de promedios y la
#'matriz de covarianza muestral
pdh_dos_medias_grandes_dados_mu_y_S<- function(n1,n2,p,xbarra1, xbarra2,s1,s2, alpha) {
  
  delta0 <- rep(0, p)
  
  matriz_enmedio <- (S1 / n1) + (S2 / n2)
  dif_vector <- xbarra1 - xbarra2 - delta0
  
  T2 <- as.numeric(t(dif_vector) %*% solve(matriz_enmedio) %*% dif_vector)
  
  chi_critico <- qchisq(1 - alpha, df = p)
  
  cat("T2 =", T2, "\n")
  cat("Chi-cuadrado =", chi_critico, "\n")
  
  if (T2 > chi_critico) {
    cat("Rechazamos H0: Hay evidencia para decir que las medias no son iguales.\n")
  } else {
    cat("No se rechaza H0: No hay suficiente evidencia para decir que las medias son distintas.\n")
  }
}


#' Encuentra los intervalos de confianza para MUESTRA GRANDE
#' dados xbarra1 y 2 y las matrices de covarianza muestral
#' checando la media de la columna 1 de la muestra 1 
#' con la media de la columna 1  de la muestra 2 y así sucesivamente
ic_diferencia_medias_grandes <- function(n1,n2,p,xbarra1, xbarra2,s1,s2, alpha) {

  var1 <- diag(S1)
  var2 <- diag(S2)
  
  chi_cuadrado <- qchisq(1 - alpha, df = p)
  
  margen_error <- sqrt(chi_cuadrado * ((var1 / n1) + (var2 / n2)))
  
  ic_inf <- (xbarra1 - xbarra2) - margen_error
  ic_sup <- (xbarra1 - xbarra2) + margen_error
  
  ic <- data.frame(
    Variable = c("x1","x2","x3","x4","x5","x6","x7"),
    IC_Lower = ic_inf,
    IC_Upper = ic_sup
  )
  
  print(ic)
}

