#1
## a
pnorm(100,mean=95,sd=sqrt(9)) - pnorm(90,mean=95,sd=sqrt(9))

## b
a <- matrix(c(3,0,4,0,-5), nrow = 5, byrow = TRUE)
a_transpuesta <- t(a)
mu <- matrix(c(100,95,230,400,86), nrow = 5, byrow = TRUE)
sigma <- matrix(c(
  10, -2,  1,  0,  3,
  -2,  9, -3,  4,  5,
  1, -3, 15,  7, -2,
  0,  4,  7, 20,  2,
  3,  5, -2,  2,  5
), nrow = 5, byrow = TRUE)
a_transpuesta %*% mu
(a_transpuesta %*% sigma) %*% a

1 - pnorm(800,mean = 790, sd = sqrt(469))

## c
mu <- matrix(c(100,95,230,400,86), nrow = 5, byrow = TRUE)
A <- matrix(c(
  1,  3, -4,  6,  1,
  2,  9, -10, 1, -1,
  0,  1,  0,  1, -1
), nrow = 3, byrow = TRUE)
Sigma <- matrix(c(
  10, -2,  1,  0,  3,
  -2,  9, -3,  4,  5,
  1, -3, 15,  7, -2,
  0,  4,  7, 20,  2,
  3,  5, -2,  2,  5
), nrow = 5, byrow = TRUE)
A %*% mu
(A %*% Sigma) %*% t(A)

## d
Sigma <- matrix(c(
  10, -2, 1, 0, 3,
  -2, 9, -3, 4, 5,
  1, -3, 15, 7, -2,
  0, 4, 7, 20, 2,
  3, 5, -2, 2, 5
), nrow = 5, byrow = TRUE)
SigmaInv <- solve(Sigma)
SigmaInv

x1menosx2 <- matrix(c(14,4,-7,-12,-5), nrow = 1, byrow = TRUE)
sqrt((x1menosx2 %*% SigmaInv) %*%  t(x1menosx2))

#2
## a
Sigma <- matrix(c(
  10, -2, 1, 0, 3,
  -2, 9, -3, 4, 5,
  1, -3, 15, 7, -2,
  0, 4, 7, 20, 2,
  3, 5, -2, 2, 5
), nrow = 5, byrow = TRUE)
Sigma_div_40 <- Sigma / 40
Sigma_div_40

pnorm(229,230,sqrt(0.375))


## b
mu <- matrix(c(100,95,230,400,86), nrow = 5, byrow = TRUE)
a <- matrix(c(4,0,3,-1,0), nrow = 5, byrow = TRUE)
a_transpuesta <- t(a)
a_transpuesta %*% mu
(a_transpuesta %*% Sigma_div_40) %*% a

pnorm(687,mean = 690, sd = sqrt(7.425))

## c
xbarramenosmu <- matrix(c(-0.5,1,1,0,0.2), nrow = 1, byrow = TRUE)
Sigma_div_40_inversa <- solve(Sigma_div_40)
sqrt((xbarramenosmu %*% Sigma_div_40_inversa) %*%  t(xbarramenosmu))

## d
mu <- matrix(c(100,95,230,400,86), nrow = 5, byrow = TRUE)
A <- matrix(c(
  1,0,2,-1,0,
  0,1,0,0,1
), nrow = 2, byrow = TRUE)

A %*% mu160
(A %*% Sigma_div_40) %*% t(A)


# 3
library(readxl)
datos <- read_excel("C:/Users/hamga/Downloads/datos tarea 3.xlsx")

media <- colMeans(datos)
covarianza <- cov(datos)

valChiCuadrada <- qchisq(ppoints(length(d2)), df = 4)

qqplot(valChiCuadrada, d2,
       xlab = "Cuantiles teóricos(chi-cuad)",
       ylab = "cuantiles muestrales (d2M)")

abline(0, 1, col = "red")  

