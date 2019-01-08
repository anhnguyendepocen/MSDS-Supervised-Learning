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

repairs <- loadDataFile("ComputerRepair.txt", sep = '\t')

y <- repairs$Units
x <- repairs$Minutes

x_hat <- mean(x)
y_hat <- mean(y)

b1 <- sum((y - y_hat) * (x - x_hat)) / sum((y - y_hat) * (y - y_hat))
b0 <- mean(x) - beta1 * mean(y)
plot(x, y)
abline(b0, b1)


fit <- lm(repairs$Units ~ repairs$Minutes)
summary(fit)
coeff = coefficients(fit)
eq = paste0("y = ", round(coeff[2], 1), "*x ", round(coeff[1], 1))

plot(x, y)
abline(fit)