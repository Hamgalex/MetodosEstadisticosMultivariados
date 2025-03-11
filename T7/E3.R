library(readxl)
library(ggplot2)
library(corrplot)
library(psych)


file_path <- "C:/Users/hamga/Downloads/datos_tarea 7.xlsx"

datos <- read_excel(file_path, sheet = 2)
head(datos)


cor_matrix <- cor(datos)
n_obs <- nrow(datos)

corrplot(cor_matrix, method = "circle", addCoef.col = "black",
         order = "original", type = "upper")


cortest.bartlett(cor_matrix, n=n_obs)

fa.parallel(cor_matrix, fm = "pa", n.obs = n_obs, ylabel = "Eigenvalues")

# sin rotar

acp <- principal(cor_matrix, nfactors = 2, rotate = "none")

print(acp)

# varimax

acp <- principal(cor_matrix, nfactors = 2, rotate = "varimax", n.obs = n_obs)

(acp$r.scores)

print(acp)

plot(acp)

# factores
mlf <- fa(cor_matrix, nfactors = 4, fm="ml", rotate = "varimax", n.obs = n_obs,
          scores = "regression")

print(mlf)

plot(mlf)



