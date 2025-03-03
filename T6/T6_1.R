

pca_matriz_correlacion <- function(datos){
  pca_corr <- prcomp(datos, center = TRUE, scale = TRUE)

  plot(pca_corr, type = "l", main = "Grafico de codo de x1, x2, ..., x6")

  print(pca_corr)

  print(summary(pca_corr))

  biplot(pca_corr, scale = 0)

  cat("eigenvalues: ", pca_corr$sdev^2 , "\n" )

  return(pca_corr)

}


library(ggplot2)
library(readxl)
file_path <- "C:/Users/hamga/Downloads/datos_tarea 6.xlsx"

datos <- read_excel(file_path, sheet = 1)
head(datos)

pca_corr <- pca_matriz_correlacion(datos)

# B

datos_valores <- data.frame(
  "Allied Chemical" = c(-0.030717, -0.003521, 0.060071),
  "Du Pont" = c(0.020202, 0.118812, 0.079646),
  "Union Carbide" = c(-0.04086, 0.089686, 0.028807),
  "Exxon" = c(-0.03905, 0.06007, 0.036666),
  "Texaco" = c(-0.05051, 0.021276, 0.026041)
)


calcular_datos_dada_pca_corr <- function(pca_corr,datos_valores){

  for (i in 1:nrow(datos_valores)) {
    cat("fila ",i,"\n")
    cat("y1=",sum(datos_valores[i,]*pca_corr$rotation[, "PC1"]),"\n")
    cat("y2=",sum(datos_valores[i,]*pca_corr$rotation[, "PC2"]),"\n\n")
  }
}

calcular_datos_dada_pca_corr(pca_corr, datos_valores)