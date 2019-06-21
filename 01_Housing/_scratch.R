library(data.table)
library(ggplot2)

lp.w <- "E:/GitHub/MSDS-RegressionAnalysis"
lp.h <- "D:/Projects/MSDS-RegressionAnalysis"

if (file.exists(lp.w)) {
  base.dir <- lp.w
} else if (file.exists(lp.h)) {
  base.dir <- lp.h
}

data.path <- paste0(base.dir, "/data/")

# simple replacement for read.csv that returns a data.table.
loadDataFile <- function(file_name, sep = ',') {
  data.raw <- fread(paste0(data.path, file_name),
    header = TRUE, sep = sep, stringsAsFactors = FALSE, na.strings = c("NA", ""))

  data <- setNames(data.raw, tools::toTitleCase(tolower(names(data.raw))))

  return(data)
}

# dataset
repairs <- loadDataFile("ComputerRepair.txt", sep = '\t')

y <- repairs$Minutes
x <- repairs$Units

x_hat <- mean(x)
y_hat <- mean(y)
n <- nrow(repairs)

# long-hand calculation

b1 <- sum((y - y_hat) * (x - x_hat)) / sum((x - x_hat) ** 2)
b0 <- mean(y) - b1 * mean(x)
plot(x, y)
abline(b0, b1)

residuals <- y - (b0 + (1:n * b1))

hist(residuals)

var(residuals)
sd(residuals)

get_estimate <- function( n = 10, x = 4 ) {
  (b0 + 1:x * b1)[n]
}

get_estimate(4) - y[4]

sd(y) / sqrt(n)

sse <- 
se <- sqrt(sse)

sum(residuals ^ 2)

b0_se <- se * ( 1 / n ) + ( x_hat **2 ) / sum( ( x - x_hat ) ** 2 )
b1_se <- se / sqrt( sum( ( x - x_hat ) ** 2) )

# Linear Model

fit <- lm(y ~ x)
summary(fit)
coeff = coefficients(fit)
eq = paste0("y = ", round(coeff[2], 1), "*x ", round(coeff[1], 1))

plot(x, y)
abline(fit)

