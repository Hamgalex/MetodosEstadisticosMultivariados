library(readxl)
library(ggplot2)
library(corrplot)
library(psych)
library(gridExtra)
library(MVN)
library(mclust)
library(clusterCrit)
library(dbscan)
library(plotly)
library(stats)
library(htmlwidgets)
library(cluster)
library(dendextend)
library(ape)
library(reshape2)

file_path <- "C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/PIA/pacientes.xlsx"
datos_original <- read_excel(file_path, sheet = 1)
nombres_pacientes <- datos_original[[2]]
datos <- datos_original[, -c(1, 2)]
datos_scaled <- scale(datos)
rownames(datos_scaled) <- nombres_pacientes

# ------------------------ ANALISIS DESCRIPTIVO ------------------------
cor_matrix <- cor(datos)

png("correlation_plot.png", width = 800, height = 600)
corrplot(cor_matrix, method = "circle", addCoef.col = "black", order = "original", type = "upper")
dev.off()

# histogramas
plots <- list()
for (var in colnames(datos)) {
  p <- ggplot(datos, aes_string(x = var)) +
    geom_histogram(aes(y = ..density..), bins = 10, fill = "blue", alpha = 0.5, color = "black") +
    geom_density(color = "red", size = 1) +
    ggtitle(paste("Histograma de", var)) +
    theme_minimal()
  plots[[var]] <- p
}
png("histogramas_pacientes.png", width = 1200, height = 800, res = 150)
grid.arrange(grobs = plots, ncol = 2)
dev.off()

# tabla de frecuencias para las 8 variables originales
cat("Tablas de frecuencias por variable original:\n")
for (var in colnames(datos)) {
  h <- hist(datos[[var]], breaks = 10, plot = FALSE)
  tabla_frec <- data.frame(
    Variable = var,
    Rango = paste(round(h$breaks[-length(h$breaks)], 2), "-", round(h$breaks[-1], 2)),
    Frecuencia = h$counts
  )
  print(tabla_frec)
}


# pruebas de normalidad univariada
resultados_normalidad <- list()
for (var in colnames(datos)) {
  shapiro_res <- shapiro.test(datos[[var]])
  resultados_normalidad[[var]] <- data.frame(Variable = var, Shapiro_p = shapiro_res$p.value)
}
df_resultados <- do.call(rbind, resultados_normalidad)
write.csv(df_resultados, "pruebas_normalidad.csv", row.names = FALSE)

# grafica para prueba de normalidad de cuantiles muestrales vs teoricos con distancia mahalanobis
media_scaled <- colMeans(datos_scaled)
covarianza_scaled <- cov(datos_scaled)
d2_scaled <- mahalanobis(datos_scaled, center = media_scaled, cov = covarianza_scaled)
valChiCuadrada_scaled <- qchisq(ppoints(length(d2_scaled)), df = ncol(datos_scaled))

png("qqplot_mahalanobis.png", width = 800, height = 600)
qqplot(valChiCuadrada_scaled, d2_scaled,
       xlab = "Cuantiles teoricos (chi-cuadrada)",
       ylab = "Cuantiles muestrales (d2M)",
       main = "Grafico QQ de distancia de Mahalanobis")
abline(0, 1, col = "red")
dev.off()

# debido a que dbclust tiene una funcion mvn tenemos que especificarle de que paquete usar para hacer la prueba de multivariada
resultado <- MVN::mvn(data = as.data.frame(datos_scaled), mvnTest = "mardia", multivariatePlot = FALSE)


# vector de promedios y desviacion muestral
vector_promedios <- colMeans(datos)
desviacion_muestral <- apply(datos, 2, sd)
cat("Vector de promedios:\n")
print(vector_promedios)
cat("Desviacion muestral:\n")
print(desviacion_muestral)

# ------------------------ ANALISIS FACTORIAL ------------------------
cor_matrix_scaled <- cor(datos_scaled)
n_obs <- nrow(datos_scaled)
bartlett_result <- cortest.bartlett(cor_matrix_scaled, n = n_obs)
print(bartlett_result)

pca_result <- prcomp(datos_scaled)
pca_summary <- summary(pca_result)
print(pca_summary)
pca_var <- pca_summary$importance[2,]
print(pca_var)

png("fa_parallel_plot.png", width = 800, height = 600)
fa.parallel(datos_scaled, fm = "pa", n.obs = n_obs, ylabel = "Eigenvalues")
dev.off()

num_factores <- 3
fa_result <- principal(datos_scaled, nfactors = num_factores, rotate = "varimax")
print(fa_result)

factores_df <- as.data.frame(fa_result$scores)
colnames(factores_df) <- c("Factor1", "Factor2", "Factor3")
factores_df$Nombre <- nombres_pacientes
write.csv(factores_df, "factores_pacientes2.csv", row.names = FALSE)

# ------------------------ CLUSTERING ------------------------
data <- as.matrix(factores_df[, 1:3])
rownames(data) <- nombres_pacientes

# kmeans
set.seed(123)
km <- kmeans(data, centers = 3)
cl_kmeans <- as.integer(km$cluster)
res_kmeans <- intCriteria(data, cl_kmeans, "C_index")
c_kmeans <- res_kmeans[[1]]
factores_df$kmeans_cluster <- as.factor(cl_kmeans)

# em
em <- Mclust(data, G = 3)
cl_em <- as.integer(em$classification)
res_em <- intCriteria(data, cl_em, "C_index")
c_em <- res_em[[1]]
factores_df$em_cluster <- as.factor(cl_em)

# jerarquico
distancia <- dist(data)
jer <- hclust(distancia, method = "complete")
cluster_jer <- cutree(jer, k = 3)
res_hc <- intCriteria(data, cluster_jer, "C_index")
c_hc <- res_hc[[1]]
factores_df$hc_cluster <- as.factor(cluster_jer)

# DBSCAN
db <- dbscan(data, eps = 1.5, minPts = 2)
valid_db <- db$cluster != 0
if (length(unique(db$cluster[valid_db])) >= 2) {
  res_db <- intCriteria(data[valid_db,], db$cluster[valid_db], "C_index")
  c_db <- res_db[[1]]
} else {
  c_db <- NA
}
factores_df$db_cluster <- as.factor(db$cluster)

# comparacion de cindex
resultados <- data.frame(
  Metodo = c("K-means", "EM", "Jerarquico", "DBSCAN"),
  Cindex = c(c_kmeans, c_em, c_hc, c_db)
)
print(resultados)


# mejor metodo es kmeans
cindex_kmeans <- numeric(10)
models <- vector("list", 10)
for (i in 1:10) {
  set.seed(i)
  km_i <- kmeans(data, centers = 3)
  models[[i]] <- km_i
  cl <- as.integer(km_i$cluster)
  res <- intCriteria(data, cl, "C_index")
  cindex_kmeans[i] <- res[[1]]
}
print(cindex_kmeans)
best_idx <- which.min(cindex_kmeans)
best_kmeans <- models[[best_idx]]
factores_df$cluster <- as.factor(best_kmeans$cluster)


# silhouette del metodo jerarquico
silhouette_scores <- numeric(10)
dist_matrix <- dist(data)
for (k in 2:10) {
  clusters <- cutree(jer, k = k)
  sil <- silhouette(clusters, dist_matrix)
  silhouette_scores[k] <- mean(sil[, 3])
}

png("silhouette_jerarquico.png", width = 800, height = 600)
plot(2:10, silhouette_scores[2:10], type = "b", pch = 19,
     xlab = "Numero de clusters (k)",
     ylab = "Promedio del indice de Silhouette",
     main = "Diagrama de Silhouette para clustering jerarquico")
dev.off()

# medias por grupo
factores_df_mean <- aggregate(. ~ cluster, data = factores_df[, c("Factor1", "Factor2", "Factor3", "cluster")], FUN = mean)
print("Medias por grupo (FACTORES):")
print(factores_df_mean)


# igualdad de medias
alpha <- 0.05
p <- 3
k <- length(unique(factores_df$cluster))
N <- nrow(factores_df)

manova_model <- manova(cbind(Factor1, Factor2, Factor3) ~ cluster, data = factores_df)

cat("\nResumen MANOVA:\n")
print(summary(manova_model, test = "Pillai"))
print(summary(manova_model, test = "Wilks"))
print(summary(manova_model, test = "Hotelling-Lawley"))
print(summary(manova_model, test = "Roy"))

wilks_lambda <- summary(manova_model, test = "Wilks")$stats[1, 2]
chi_critico <- qchisq(1 - alpha, df = p * (k - 1))
bartlett_stat <- -(N - 1 - (p + k) / 2) * log(wilks_lambda)

cat("\nEstadistico de prueba (chi-cuadrada):", bartlett_stat, "\n")
cat("Valor critico chi-cuadrada:", chi_critico, "\n")

if (bartlett_stat > chi_critico) {
  cat("Rechazamos H0: Hay evidencia para decir que las medias no son iguales.\n")
} else {
  cat("No se rechaza H0: No hay suficiente evidencia para decir que las medias son distintas.\n")
}

# ------------------------ ANALISIS DESCRIPTIVO DE FACTORES ------------------------
cat("Resumen estadístico de los factores:\n")
print(summary(factores_df[, 1:3]))

# Histogramas con tabla de frecuencias para Factor1
hist_f1 <- hist(factores_df$Factor1, breaks = 10, plot = FALSE)
tabla_frec <- data.frame(
  Rango = paste(round(hist_f1$breaks[-length(hist_f1$breaks)], 2), "-", round(hist_f1$breaks[-1], 2)),
  Frecuencia = hist_f1$counts
)
cat("Tabla de frecuencias para Factor 1:\n")
print(tabla_frec)

png("histograma_factor1.png", width = 800, height = 600)
plot(hist_f1, col = "lightblue", main = "Histograma de Factor 1", xlab = "Factor 1")
dev.off()


# ------------------------ NORMALIDAD EN LOS FACTORES ------------------------
cat("\n\nPrueba de normalidad univariada en los factores (Shapiro-Wilk):\n")
for (f in colnames(factores_df)[1:3]) {
  test <- shapiro.test(factores_df[[f]])
  cat(f, ": p-valor =", test$p.value, "\n")
}

cat("\nPrueba de normalidad multivariada (Mardia) sobre los factores:\n")
mvn_factores <- MVN::mvn(data = factores_df[, 1:3], mvnTest = "mardia", multivariatePlot = TRUE)
print(mvn_factores$multivariateNormality)
