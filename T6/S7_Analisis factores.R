# genera la sesion de trabajo
file_path <- "C:/Users/hamga/Downloads/S7_datos_vendedor.csv"

# cargar datos

datos <- read.csv(file_path, header = T)

datos <- datos[,-1]

# prueba de normalidad multivariante

library(MVN)

mvn(datos, mvnTest = "mardia") # los datos no son normales multivariados

# Analizando la correlacion entre las variables

library(ggplot2)

library(corrplot)

datos.cor <- round(cor(datos, method = "pearson"), digits = 2)

corrplot(datos.cor, method = "circle", addCoef.col = "black", 
         order = "original", type = "upper")

# se recomienda estandarizar los datos si las unidades son diferentes, 

datos.st <- scale(datos)

library(psych)

# H0: Corr(Xi, Xj)=0  vs  H1: Corr(Xi, Xj) =! 0

correlaciones <- corr.test(datos) # inferencia para la correlacion

print(correlaciones)

R <- as.matrix(correlaciones$r)

# prueba de Correlacion (Prueba de Esfericidad de Bartlett)
# Ho: La correlacion entre cada par de variables es cero
# H1: La correlacion entre cada par de variable diferente de cero

cortest.bartlett(R, n=50)

# se infiere que si hay correlacion entre las variables

# Determinar el numero de factores

# Metodo de componentes principales ----

fa.parallel(R, fm = "pa", n.obs = 50, ylabel = "Eigenvalues")

# se recomiendan 2 factores

com_principales <- prcomp(datos)

plot(com_principales, type = "l")

# proporcion de varianza explicada por cada componente

summary(com_principales)

acp <- principal(R, nfactors = 2, rotate = "none")

print(acp)

acp <- principal(R, nfactors = 2, rotate = "varimax", n.obs = 50)

(acp$r.scores)

print(acp)

# h2: comunalidad de cada variable (varianza comun explicada)
# u2: varianza especifica (varianza no explicada)
# SS loading: suma de h2
# Proportion Var: % de Varianza explcada del total
# RMSR; Media de los cuadrados de los residuales 

# Metodo de maxima verosimilitud ----

mlf <- fa(datos, nfactors = 2, fm="ml", rotate = "varimax", n.obs = 50, 
          scores = "regression")

print(mlf)

mlf$scores

# representacion grafica

plot(acp, labels = row.names(R))

plot(mlf, labels = row.names(R))

# puntuaciones

mlf <- fa(datos, nfactors = 2, fm="ml", rotate = "varimax", 
          scores = "regression")

acp <- principal(datos, nfactors = 2, rotate = "varimax", scores = T, n.obs = 50,
                 method = "regression")

# matriz de cargas factoriales

L.acp <- acp$loadings

L.mlf <- mlf$loadings

#  puntuaciones para los 2 metodos

scores.acp <- acp$scores

scores.mlf <- mlf$scores

summary(scores.acp); summary(scores.mlf)

var(scores.acp); var(scores.mlf)

# Factores No correlacionados

corr.test(scores.acp)

corr.test(scores.mlf)
