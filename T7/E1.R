
library(ggplot2)
library(corrplot)
library(psych)

cor_matrix <- matrix(c(1, 0.83, 0.81, 0.8, 0.71, 0.54,
                       0.83, 1, 0.87, 0.62, 0.59, 0.58,
                       0.81, 0.87, 1, 0.63, 0.37, 0.3,
                       0.8, 0.62, 0.63, 1, 0.49, 0.3,
                       0.71, 0.59, 0.37, 0.49, 1, 0.34,
                       0.54, 0.58, 0.3, 0.3, 0.34, 1),
                     nrow = 6, ncol = 6, byrow = TRUE)


n_obs <- 100

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
mlf <- fa(cor_matrix, nfactors = 2, fm="ml", rotate = "varimax", n.obs = n_obs,
          scores = "regression")

print(mlf)

mlf$scores




plot(mlf)