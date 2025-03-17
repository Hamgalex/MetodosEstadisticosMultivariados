library(readxl)
library(ggplot2)
library(corrplot)
library(psych)


file_path <- "C:/Users/hamga/Documents/maestria/2do/MEM/MetodosEstadisticosMultivariados/PIA/pacientes.xlsx"

datos <- read_excel(file_path, sheet = 1)
datos <- datos[, -c(1, 2)]
datos
cor_matrix <- cor(datos)
n_obs <- nrow(datos)


corrplot(cor_matrix, method = "circle", addCoef.col = "black",
         order = "original", type = "upper")

bartlett_result <- cortest.bartlett(cor_matrix, n=n_obs)
print(bartlett_result)

fa.parallel(cor_matrix, fm = "pa", n.obs = n_obs, ylabel = "Eigenvalues")

mlf <- fa(cor_matrix, nfactors = 3, fm="ml", n.obs = n_obs,
          scores = "regression")
mlf$chi
mlf$dof
mlf$PVAL
print(mlf)

plot(mlf)