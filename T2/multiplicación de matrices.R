A <- matrix(c(3, -2, 5, 9, 6, -8, 4, 1, -1), nrow = 3, byrow = TRUE)
B <- matrix(c(12, 5, 12, 5, 16, 15, 12, 15, 20), nrow = 3, byrow = TRUE)
C <- matrix(c(3, 9, 4, -2, 6, 1, 5, -8, -1), nrow = 3, byrow = TRUE)

# Multiplicar las matrices
result_AB <- A %*% B
result_ABC <- result_AB %*% C

# Imprimir los resultados
cat("Resultado de A * B:\n")
print(result_AB)

cat("\nResultado de (A * B) * C:\n")
print(result_ABC)