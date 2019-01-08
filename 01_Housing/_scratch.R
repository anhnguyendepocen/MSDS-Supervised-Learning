library(data.table)
library(ggplot2)

path.work <- "E:/GitHub/MSDS-RegressionAnalysis/data"
path.home <- "D:/Projects/MSDS-RegressionAnalysis/data"

if (file.exists(path.home)) {
  setwd(path.home)
} else {
  setwd(path.work)
}

repairs <- read.csv("ComputerRepair.txt", sep = "\t")

x_hat <- mean(repairs$Minutes)
y_hat <- mean(repairs$Units)

sum((repairs$Units - y_hat) * (repairs$Minutes - x_hat))

sum((repairs$Minutes - x_hat)) ^ 2

fit <- lm(repairs$Units ~ repairs$Minutes)
summary(fit)

plot(repairs)
abline(fit)
