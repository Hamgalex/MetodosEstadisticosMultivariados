
datos <- read_excel("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/T5/datos_tarea.xlsx", sheet=7)


n1 <- sum(datos$universidad == "A")
n2 <- sum(datos$universidad == "B")
n3 <- sum(datos$universidad == "C")
N=nrow(datos)
p=ncol(datos)-1
k=3

manova_result <- manova(cbind(x1, x2, x3, x4, x5, x6) ~ universidad, data = datos)

summary(manova_result, test = "Pillai")
summary(manova_result, test = "Wilks")
summary(manova_result, test = "Hotelling-Lawley")
summary(manova_result, test = "Roy")

valor_chi <- qchisq(1 - alpha, df = p * (k - 1))
valor_chi
bartlett =- (N-1-(p+k)/2)*log(summary(manova_result, test = "Wilks")$stats[1,2])
bartlett

if (bartlett>valor_chi) {
  cat("Rechazamos H0: Hay evidencia para decir que las medias no son iguales.\n")
} else {
  cat("No se rechaza H0: No hay suficiente evidencia para decir que las medias son distintas.\n")
}


list_of_dfs <- split(datos, datos$universidad)

muestra1 <- list_of_dfs[["A"]]
muestra2 <- list_of_dfs[["B"]]
muestra3 <- list_of_dfs[["C"]]

muestra1$universidad <- NULL
muestra2$universidad <- NULL
muestra3$universidad <- NULL

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

pdh_m_de_box(muestra1,muestra2,muestra3,0.05)


