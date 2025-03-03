A <- matrix(c(2,0,7,8,6,4,6,-2,0,7,9,2,6,5,4,-8), ncol = 4, byrow=TRUE)
B <- matrix(c(2,0,-5,3,6,9,0,-2),ncol=2, byrow=TRUE)
C <- matrix(c(3,-3,6,9,4,0,-2,0),ncol=4, byrow=TRUE)
A
B
C

# B'A
t(B) %*% A

# 3BC + 4A
3 * B %*% C + 4*A

# 5C' - 6B
5 * t(C) - 6* B

x <- c(2,5,-3,1,-1)
norm(as.matrix(x), type = "2")

