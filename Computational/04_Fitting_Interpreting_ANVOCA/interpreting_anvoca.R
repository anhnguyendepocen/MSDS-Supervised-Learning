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
library(lessR)

#####################################################################
######################### Assignment 4 ##############################
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
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ", signif(summary(fit)$adj.r.squared, 5),
                      "Intercept =", signif(fit$coef[[1]], 5),
                      " Slope =", signif(fit$coef[[2]], 5),
                      " P =", signif(summary(fit)$coef[2, 4], 5)))
}

# Data of interest

data.nutrition <- as.data.table(read.csv(file = "NutritionStudy.csv", head = TRUE, sep = ","))

ggplot(data.nutrition, aes(Fiber, Cholesterol)) +
  geom_point()

cor(data.nutrition$Cholesterol, data.nutrition$Fiber)

model1_fit <- lm(formula = Cholesterol ~ Fiber, data = data.nutrition)
summary(model1_fit)
anova(model1_fit)

ggplotRegression(model1_fit)

coef(model1_fit)

round(model1_fit$coefficients, 4)

summary(model1_fit)$r.squared * 100

# Model 2

# Dummy Coded

data.nutrition$AlcoholUse <- ifelse(data.nutrition$Alcohol == 0, "None", ifelse(data.nutrition$Alcohol < 10, "Moderate", "Heavy"))
data.nutrition$AlcoholUse <- factor(data.nutrition$AlcoholUse, labels = c("Heavy", "Moderate", "None"))
levels(data.nutrition$AlcoholUse) <- c("None", "Moderate", "Heavy")
contrasts(data.nutrition$AlcoholUse) = matrix(c(-1, 1, 0, -1, 0, 1), ncol = 2)

tapply(data.nutrition$Cholesterol, data.nutrition$AlcoholUse, mean)

alcohol <- model.matrix(~AlcoholUse, data = data.nutrition)

head(alcohol)

model2_data <- data.table(Cholesterol = data.nutrition$Cholesterol, AlcoholUse = data.nutrition$AlcoholUse, alcohol[, 2:3], Fiber = data.nutrition$Fiber)

model2_fit <- lm(formula = Cholesterol ~ AlcoholUse + Fiber, data = model2_data)
model2_fit <- lm(formula = Cholesterol ~ AlcoholUseModerate + AlcoholUseHeavy + Fiber, data = model2_data)
summary(model2_fit)
anova(model2_fit)

ggplotRegression(model2_fit)

