# code to test the cachematrix functions

x <- c(4, 2, 7, 6)
x <- matrix(x, nrow = 2)
x
solve(x)
x * solve(x)
x %*% solve(x)

y <- makeCacheMatrix(x)
cacheSolve(y)
