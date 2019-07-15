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

# Theme Overrides
theme_update(plot.title = element_text(hjust = 0.5),
             axis.text.x = element_text(size = 10),
             axis.text.y = element_text(size = 10),
             axis.title = element_text(face = "bold", size = 12, colour = "steelblue4"),
             legend.position = "top", legend.title = element_blank())

# Utility

ggplotRegression <- function(fit) {
  p1 <- ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ", signif(summary(fit)$adj.r.squared, 5),
                      "Intercept =", signif(fit$coef[[1]], 5),
                      " Slope =", signif(fit$coef[[2]], 5),
                      " P =", signif(summary(fit)$coef[2, 4], 5)))
  
  res <- data.table(Value = residuals(fit))
  res.norm <- data.table(Value = (res$Value - mean(res$Value)) / sd(res$Value))

  p2 <- ggplot(res, aes(Value, fill = ..count..)) +
    geom_histogram(breaks = pretty(res.norm$Value)) +
    labs(title = "Residuals")

  grid.arrange(p1, p2)
}

# Data of interest

data.nutrition <- as.data.table(read.csv(file = "NutritionStudy.csv", head = TRUE, sep = ","))

# Recode Variables
unique(data.nutrition$VitaminUse)
data.nutrition$VitaminCoded <- as.factor(data.nutrition$VitaminUse)
levels(data.nutrition$VitaminCoded) = c(0, 1, 2)

data.nutrition$VitaminCoded2 <- as.factor(data.nutrition$VitaminUse)
levels(data.nutrition$VitaminCoded2) = c(3, 2, 1)

unique(data.nutrition$Gender)
data.nutrition$GenderCoded <- as.factor(data.nutrition$Gender)
levels(data.nutrition$GenderCoded) = c(0, 1)

data.nutrition$SmokeCoded2 <- as.factor(data.nutrition$Smoke)
levels(data.nutrition$SmokeCoded2) <- c(0, 1)

str(data.nutrition)

# Linear Model #1

model_data <- data.nutrition[, .(VitaminUse, VitaminCoded, VitaminCoded2, Cholesterol)]
model1_fit <- lm(formula = Cholesterol ~ VitaminUse, data = model_data)

summary(model1_fit)

ggplotRegression(model1_fit)

model2_fit <- lm(formula = Cholesterol ~ VitaminCoded, data = model_data)

summary(model2_fit)
ggplotRegression(model2_fit)

model3_fit <- lm(formula = Cholesterol ~ VitaminCoded2, data = model_data)

summary(model3_fit)
ggplotRegression(model3_fit)

