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

fVsC <- cor(data.nutrition$Cholesterol, data.nutrition$Fiber)
fVsC <- round(fVsC, 4)

ggplot(data.nutrition, aes(Fiber, Cholesterol)) +
  geom_point() +
  labs(title = paste("Correlation: ", fVsC))

model1_fit <- lm(formula = Cholesterol ~ Fiber, data = data.nutrition)
summary(model1_fit)
anova(model1_fit)

ggplotRegression(model1_fit)

coef(model1_fit)

round(model1_fit$coefficients, 4)

summary(model1_fit)$r.squared * 100

# Model 2

# Dummy Coded Alcohol

data.nutrition$AlcoholUse <- ifelse(data.nutrition$Alcohol == 0, "None", ifelse(data.nutrition$Alcohol < 10, "Moderate", "Heavy"))
data.nutrition$AlcoholUse <- factor(data.nutrition$AlcoholUse, labels = c("Heavy", "Moderate", "None"))
levels(data.nutrition$AlcoholUse) <- c("None", "Moderate", "Heavy")
contrasts(data.nutrition$AlcoholUse) = matrix(c(-1, 1, 0, -1, 0, 1), ncol = 2)

tapply(data.nutrition$Cholesterol, data.nutrition$AlcoholUse, mean)

data.nutrition[, .(Count = .N), by = AlcoholUse]

ggplot(data.nutrition, aes(Cholesterol, AlcoholUse)) +
  geom_boxplot(outlier.colour = "blue", outlier.shape = 1) +
  geom_jitter(width = 0.2) +
  coord_flip()

alcohol <- model.matrix(~AlcoholUse, data = data.nutrition)

head(alcohol)

model2_data <- data.table(Cholesterol = data.nutrition$Cholesterol, alcohol[, 2:3], Fiber = data.nutrition$Fiber)

round(summary(model2_fit)$r.squared * 100, 4)

model2_fit <- lm(formula = Cholesterol ~ Fiber + AlcoholUseModerate + AlcoholUseHeavy, data = model2_data)
summary(model2_fit)
anova(model2_fit)

round(coef(model2_fit), 4)

ggplot(model2_fit$model, aes_string(x = names(model2_fit$model)[2], y = names(model2_fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ", signif(summary(model2_fit)$adj.r.squared, 5),
                      "Intercept =", signif(model2_fit$coef[[1]], 5),
                      " Slope =", signif(model2_fit$coef[[2]], 5),
                      " P =", signif(summary(model2_fit)$coef[2, 4], 5)))

alpha1 <- 0.01
beta1 <- 2.868

t.val <- beta1 / 0.4104
t.val <- round(T, 4)

c(beta1 - alpha1, beta1 + alpha1)

crit.value <- abs(qt(alpha1 / 2, 72 - 4 - 2)) # 99% confidence, 2 sided
round(crit.value, 4)

data.nutrition$y_hat <- predict(model2_fit)

ggplot(data.nutrition, aes(Fiber, y_hat, color = AlcoholUse)) +
  geom_point()

ggplot(data.nutrition, aes(Fiber, Cholesterol, color = AlcoholUse)) +
  geom_point()

# Model 3

model3_data <- data.table(Cholesterol = data.nutrition$Cholesterol, AlcoholUse = data.nutrition$AlcoholUse, alcohol[, 2:3], Fiber = data.nutrition$Fiber)
model3_data$FiberModerate <- model3_data$Fiber * model3_data$AlcoholUseModerate
model3_data$FiberHeavy <- model3_data$Fiber * model3_data$AlcoholUseHeavy

model3_fit <- lm(formula = Cholesterol ~ Fiber + AlcoholUseModerate + AlcoholUseHeavy + FiberModerate + FiberHeavy, data = model3_data)
summary(model3_fit)
coef(model3_fit)

model3_data$pred <- predict(model3_fit)

ggplot(model3_data) +
  geom_point(aes(Fiber, pred, color = AlcoholUse)) +
  geom_point(aes(Fiber, Cholesterol, color = AlcoholUse, alpha = .5))

ggplot(model3_data, aes(Fiber, Cholesterol, color = AlcoholUse)) +
  geom_point()
