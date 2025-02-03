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

