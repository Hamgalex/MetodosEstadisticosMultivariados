library(ggplot2)
library(readxl)
file_path <- "C:/Users/hamga/Downloads/datos_tarea 6.xlsx"

datos <- read_excel(file_path, sheet = 2)
head(datos)

pca_corr <- pca_matriz_correlacion(datos)

# B
datos_valores <- data.frame(
  X1 = c(5, 7, 7),
  X2 = c(86, 79, 79),
  X3 = c(7, 7, 5),
  X4 = c(2, 4, 2),
  X5 = c(13, 9, 8),
  X6 = c(18, 25, 6),
  X7 = c(2, 3, 2)
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

