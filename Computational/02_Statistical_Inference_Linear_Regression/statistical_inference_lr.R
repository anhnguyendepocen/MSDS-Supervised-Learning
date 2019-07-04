library(data.table)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(GGally)
library(ggthemes)
library(formattable)
library(scales)
library(reshape2)
library(skimr)
library(gridExtra)

#####################################################################
######################### Assignment 3 ##############################
#####################################################################

path.work <- "E:/GitHub/MSDS-RegressionAnalysis/data"
path.home <- "D:/Projects/MSDS-RegressionAnalysis/data"

if (file.exists(path.home)) {
  setwd(path.home)
} else {
  setwd(path.work)
}

theme_set(theme_light())

ssr <- 1974.53 + 118.8642568 + 32.47012585 + 0.435606985
sse <- 630.36
sst <- ssr + sse

rsq <- ssr / sst
#rsq <- round(rsq, 4)

n <- 72
adj_r2 <- 1 - ((1 - rsq) * (n - 1) / (n - 4 - 1))

round(adj_r2, 4)

# F-test

n <- 72
p <- 4

ssr <- 1974.53 + 118.8642568 + 32.47012585 + 0.435606985
sse <- 630.36
sst <- ssr + sse

F <- ((sst - sse) / p) / (sse / (n - p - 1))
round(F, 4)

p.val <- df(F, p, n - p - 1)

round(p.val, 4)
