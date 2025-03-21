library(readxl)
library(ggplot2)
library(corrplot)
library(psych)

# Cargar los datos
file_path <- "C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/PIA/pacientes.xlsx"
datos <- read_excel(file_path, sheet = 1)
datos <- datos[, -c(1, 2)]  # Eliminar las primeras dos columnas si no son relevantes

# Normalizar los datos (escala de los datos)
datos_scaled <- scale(datos)

# Matriz de correlación
cor_matrix <- cor(datos_scaled)
n_obs <- nrow(datos)

# Prueba de Bartlett
bartlett_result <- cortest.bartlett(cor_matrix, n = n_obs)
print(bartlett_result)

# Interpretación de la prueba de Bartlett
# Si el valor de p es pequeño (p < 0.05), la hipótesis nula se rechaza, lo que sugiere que la técnica de análisis de factores es adecuada

# Análisis de Componentes Principales (PCA)
pca_result <- prcomp(datos_scaled)

# Resumen de los componentes principales
pca_summary <- summary(pca_result)
print(pca_summary)

# Tabla con la varianza explicada por cada componente principal
pca_var <- pca_summary$importance[2, ]
print(pca_var)

# Realizar análisis de componentes principales y determinar el número de factores
png("fa_parallel_plot.png", width = 800, height = 600)

# Realizar análisis de factores con fa.parallel y guardar el gráfico
fa.parallel(datos_scaled, fm = "pa", n.obs = n_obs, ylabel = "Eigenvalues")

# Cerrar el dispositivo gráfico para guardar el archivo
dev.off()


# Obtener la matriz de factores (componentes principales) para los factores seleccionados
# Seleccionaremos el número de factores basado en el gráfico de codo o la varianza explicada
num_factores <- 3  # Este número debe basarse en la interpretación del gráfico de codo o la varianza explicada

# Rotación de factores usando el método varimax
fa_result <- principal(datos_scaled, nfactors = num_factores, rotate = "varimax")
print(fa_result)


# Extraer los scores (las 3 nuevas variables)
factores_df <- as.data.frame(fa_result$scores)

# Cambiar nombres de columnas si lo deseas
colnames(factores_df) <- c("Factor1", "Factor2", "Factor3")


write.csv(factores_df, file = "factores_pacientes.csv", row.names = FALSE)
