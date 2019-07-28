data(mtcars)

head(mtcars)

y <- mtcars$mpg
x <- cbind(1, mtcars$hp, mtcars$wt)

head(x)

solve(t(x) %*% x) %*% t(x) %*% y # Calculate coefficents for Y ~ X

# Verify
fit <- lm(mpg ~ hp + wt, data = mtcars)

coef(fit)

# Mean centered X (hat Matrix)
n <- nrow(x)
I <- diag(rep(1, n))
H <- matrix(1, n, n) / n

xc <- (I - H) %*% x
apply(xc, 2, mean)

# Alternative calculation
xc2 <- sweep(x, 2, apply(x, 2, mean))
apply(xc2, 2, mean)

# Calculate covariance matrix

cov <- (t(x) %*% (I - H) %*% x) / (n - 1)
round(cov, 6)

var(x)
