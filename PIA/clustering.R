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