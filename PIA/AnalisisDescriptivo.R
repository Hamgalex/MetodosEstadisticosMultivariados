library(readxl)
library(ggplot2)
library(corrplot)
library(psych)
library(ggplot2)
library(gridExtra)
library(MVN)

file_path <- "C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/PIA/pacientes.xlsx"

datos <- read_excel(file_path, sheet = 1)
datos <- datos[, -c(1, 2)]
datos
cor_matrix <- cor(datos)
n_obs <- nrow(datos)


png("correlation_plot.png", width = 800, height = 600)

# Generar el gráfico de correlación
corrplot(cor_matrix, method = "circle", addCoef.col = "black",
         order = "original", type = "upper")

# Cerrar el dispositivo gráfico para guardar la imagen
dev.off()




# Crear lista de gráficos
plots <- list()
variables <- colnames(datos)

for (var in variables) {
  p <- ggplot(datos, aes_string(x = var)) +
    geom_histogram(aes(y = ..density..), bins = 10, fill = "blue", alpha = 0.5, color = "black") +
    geom_density(color = "red", size = 1) +
    ggtitle(paste("Histograma de", var)) +
    theme_minimal()
  
  plots[[var]] <- p
}

# Guardar la imagen con los histogramas en una cuadrícula
output_file <- "histogramas_pacientes.png"
png(output_file, width = 1200, height = 800, res = 150)
grid.arrange(grobs = plots, ncol = 2)
dev.off()

cat("Imagen guardada como", output_file, "\n")



# Inicializar lista para almacenar los resultados
resultados_normalidad <- list()


# Pruebas de normalidad para cada variable
for (var in colnames(datos)) {
  x <- datos[[var]]
  
  # Shapiro-Wilk test
  shapiro_res <- shapiro.test(x)
  
  # Guardar resultados en lista
  resultados_normalidad[[var]] <- data.frame(
    Variable = var,
    Shapiro_p = shapiro_res$p.value
  )
}

# Combinar los resultados en un solo data frame
df_resultados <- do.call(rbind, resultados_normalidad)
print(df_resultados)

# Guardar resultados en un archivo CSV
write.csv(df_resultados, "pruebas_normalidad.csv", row.names = FALSE)
cat("Resultados guardados en pruebas_normalidad.csv\n")



datos_scaled <- scale(datos)

# Calcular la media y la matriz de covarianza para los datos estandarizados
media_scaled <- colMeans(datos_scaled)
covarianza_scaled <- cov(datos_scaled)

# Cálculo de la distancia de Mahalanobis para los datos estandarizados
d2_scaled <- mahalanobis(datos_scaled, center = media_scaled, cov = covarianza_scaled)

# Cálculo de los cuantiles de la distribución chi-cuadrada
valChiCuadrada_scaled <- qchisq(ppoints(length(d2_scaled)), df = ncol(datos_scaled))

# Gráfico QQ plot
qqplot(valChiCuadrada_scaled, d2_scaled,
       xlab = "Cuantiles teóricos (chi-cuadrada)",
       ylab = "Cuantiles muestrales (d2M)",
       main = "Gráfico QQ de la distancia de Mahalanobis (Datos Estandarizados)")
abline(0, 1, col = "red")  # Línea de referencia (roja)



# Aplicar la prueba de normalidad multivariada
resultado <- mvn(datos_scaled, mvnTest = "mardia")

# Ver los resultados
print(resultado)



vector_promedios <- colMeans(datos)

# Calcular la desviación muestral para cada variable
desviacion_muestral <- apply(datos, 2, sd)

# Mostrar los resultados
cat("Vector de Promedios:\n")
print(vector_promedios)
cat("\nDesviación Muestral para cada variable:\n")
print(desviacion_muestral)




