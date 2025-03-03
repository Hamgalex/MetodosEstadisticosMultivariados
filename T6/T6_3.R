library(ggplot2)
library(readxl)
file_path <- "C:/Users/hamga/Downloads/datos_tarea 6.xlsx"

datos <- read_excel(file_path, sheet = 3)
head(datos)

pca_corr <- pca_matriz_correlacion(datos)

#b

datos_valores <- data.frame(
  x1 = c(58, 50, 60),
  x2 = c(62, 62, 57),
  x3 = c(58, 54, 54),
  x4 = c(34, 35, 32),
  x5 = c(50, 49, 45),
  x6 = c(0.1, 0.1, 0.065),
  x7 = c(0.033, 0.036, 0.3),
  x8 = c(0.097, 0.099, 0.062),
  x9 = c(120, 120, 120)
)

calcular_datos_dada_pca_corr <- function(pca_corr,datos_valores, medias, desvest){

  for (i in 1:nrow(datos_valores)) {

    # se tienen que escalar los valores debido a que para el pca
    # se uso la correlacion en vez de la covarianza.
    datos_valores_escalados <- scale(datos_valores[i,], center = medias, scale = desvest)

    cat("fila ",i,"\n")
    cat("y1=",sum(datos_valores_escalados*pca_corr$rotation[, "PC1"]),"\n")
    cat("y2=",sum(datos_valores_escalados*pca_corr$rotation[, "PC2"]),"\n")
    cat("y3=",sum(datos_valores_escalados*pca_corr$rotation[, "PC3"]),"\n")
    cat("y4=",sum(datos_valores_escalados*pca_corr$rotation[, "PC4"]),"\n\n")
  }
}

medias <- apply(datos, 2, mean)
desvest <- apply(datos, 2, sd)
calcular_datos_dada_pca_corr(pca_corr, datos_valores, medias,desvest)