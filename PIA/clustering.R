library(plotly)
library(htmlwidgets)

# Leer los factores
factores_df <- read.csv("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/PIA/factores_pacientes.csv")

# K-means
set.seed(123)
data <- as.matrix(factores_df[, 1:3])
km <- kmeans(data, centers = 3, nstart = 10)
factores_df$cluster <- as.factor(km$cluster)

# Crear el gráfico 3D interactivo
p <- plot_ly(factores_df,
             x = ~Factor1,
             y = ~Factor2,
             z = ~Factor3,
             color = ~cluster,
             colors = c("red", "blue", "green"),
             type = "scatter3d",
             mode = "markers",
             marker = list(size = 5)) %>%
  layout(title = "Clusters en espacio 3D (K-means)",
         scene = list(
           xaxis = list(title = "Factor 1"),
           yaxis = list(title = "Factor 2"),
           zaxis = list(title = "Factor 3")
         ))

# Guardar el gráfico como HTML interactivo
saveWidget(p, file = "C:/Users/hamga/Documents/clusters_3d_interactivo.html", selfcontained = TRUE)


# Instalar si no lo tienes
# install.packages("mclust")
# install.packages("clusterCrit")
# install.packages("plotly")

library(mclust)
library(clusterCrit)
library(plotly)

# Leer los datos
factores_df <- read.csv("C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/PIA/factores_pacientes.csv")
data <- as.matrix(factores_df[, 1:3])

# Aplicar EM con 3 grupos
em_result <- Mclust(data, G = 3)

# Guardar clasificación
factores_df$cluster <- as.factor(em_result$classification)

# Calcular C-index
cindex_em <- intCriteria(data, as.integer(em_result$classification), "C_index")
cat("🔹 C-index (EM):", cindex_em$C_index, "\n")

# Caracterización por grupo
cat("\n📊 Medias por grupo (EM):\n")
print(aggregate(. ~ cluster, data = factores_df, mean))

cat("\n📈 Varianzas por grupo (EM):\n")
print(aggregate(. ~ cluster, data = factores_df, var))

cat("\n📐 Rangos por grupo (EM):\n")
rango <- function(x) diff(range(x))
print(aggregate(. ~ cluster, data = factores_df, FUN = rango))

# Visualización 3D interactiva
plot_ly(factores_df,
        x = ~Factor1,
        y = ~Factor2,
        z = ~Factor3,
        color = ~cluster,
        colors = c("red", "blue", "green"),
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 5)) %>%
  layout(title = "Clusters en espacio 3D (EM - Expectation Maximization)",
         scene = list(
           xaxis = list(title = "Factor 1"),
           yaxis = list(title = "Factor 2"),
           zaxis = list(title = "Factor 3")
         ))