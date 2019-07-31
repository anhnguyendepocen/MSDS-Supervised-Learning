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
library(broom)
library(sigr)
library(WVPlots)

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
  p1 <- ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ", signif(summary(fit)$adj.r.squared, 5),
                      "Intercept =", signif(fit$coef[[1]], 5),
                      " Slope =", signif(fit$coef[[2]], 5),
                      " P =", signif(summary(fit)$coef[2, 4], 5)))

  res <- as.data.table(residuals(fit))
  colnames(res) <- c("value")

  p2 <- ggplot(res, aes(value, fill = ..count..)) +
    geom_histogram(breaks = pretty(res$value)) +
    labs(title = "Residuals")

  grid.arrange(p1, p2)
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

glance(model1_fit)
wrapFTest(model1_fit)

mean(data.nutrition$Cholesterol)
mean(data.nutrition$Fiber)

193.701 + 12.78857 * 3.813

ggplotRegression(model1_fit)

coef(model1_fit)

round(model1_fit$coefficients, 4)

summary(model1_fit)$r.squared * 100

# Model 2

# Dummy Coded Alcohol

data.nutrition$AlcoholUse <- ifelse(data.nutrition$Alcohol == 0, "None", ifelse(data.nutrition$Alcohol < 10, "Moderate", "Heavy"))
data.nutrition$AlcoholUse <- factor(data.nutrition$AlcoholUse, labels = c("Heavy", "Moderate", "None"), levels = )
levels(data.nutrition$AlcoholUse) <- c("None", "Moderate", "Heavy")

tapply(data.nutrition$Cholesterol, data.nutrition$AlcoholUse, mean)

data.nutrition[, .(Count = .N), by = AlcoholUse]

ggplot(data.nutrition, aes(Cholesterol, AlcoholUse)) +
  geom_boxplot(outlier.colour = "blue", outlier.shape = 1) +
  geom_jitter(width = 0.2) +
  coord_flip()

alcohol <- model.matrix(~AlcoholUse, data = data.nutrition)

head(alcohol)

contrasts(data.nutrition$AlcoholUse) = matrix(c(-1, 1, 0, -1, 0, 1), ncol = 2)

model2_data <- data.table(Cholesterol = data.nutrition$Cholesterol, AlcoholUse = data.nutrition$AlcoholUse, alcohol[, 2:3], Fiber = data.nutrition$Fiber)
model2_fit <- lm(formula = Cholesterol ~ Fiber + AlcoholUseModerate + AlcoholUseHeavy, data = model2_data)

summary(model2_fit)
anova(model2_fit)

round(summary(model2_fit)$r.squared * 100, 4)
round(coef(model2_fit), 4)

ggplotRegression(model2_fit)

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

model2_data$pred <- predict(model2_fit)

ggplot(model2_data, aes(Fiber, pred, color = AlcoholUse)) +
  geom_point()

GainCurvePlot(model2_data, "pred", "Cholesterol", "Cholesterol model")

ggplot(data.nutrition, aes(Fiber, Cholesterol, color = AlcoholUse)) +
  geom_point()

# Model 3

model3_data <- data.table(Cholesterol = data.nutrition$Cholesterol, AlcoholUse = data.nutrition$AlcoholUse, alcohol[, 2:3], Fiber = data.nutrition$Fiber)
model3_data$FiberModerate <- model3_data$Fiber * model3_data$AlcoholUseModerate
model3_data$FiberHeavy <- model3_data$Fiber * model3_data$AlcoholUseHeavy

model3_fit <- lm(formula = Cholesterol ~ Fiber + AlcoholUseModerate + AlcoholUseHeavy + FiberModerate + FiberHeavy, data = model3_data)
summary(model3_fit)

round(coef(model3_fit), 4)

round(summary(model3_fit)$r.squared, 4) * 100

model3_data$pred <- predict(model3_fit)

GainCurvePlot(model3_data, "pred", "Cholesterol", "Cholesterol model")

ggplot(model3_data) +
  geom_point(aes(Fiber, pred, color = AlcoholUse)) +
  geom_point(aes(Fiber, Cholesterol, color = AlcoholUse, alpha = .5))

ggplot(model3_data, aes(Fiber, Cholesterol, color = AlcoholUse)) +
  geom_point()

partial_f_test(model3_fit, model2_fit)

m1_aov <- anova(model3_fit)
m2_aov <- anova(model2_fit)

df_1 <- 4
df_2 <- 2

ss_f <- m1_aov$`Sum Sq`[length(m1_aov$`Sum Sq`)]

ss_r <- m2_aov$`Sum Sq`[length(m2_aov$`Sum Sq`)]
msf <- sum(m1_aov$`Mean Sq`)

n <- nrow(data.nutrition)

k <- 3
p <- 5

f.val <- ((ss_r - ss_f) / k) / (ss_f / (n - (p + 1)))
round(f.val, 4)

dff <- 309
dfr <- 311
dfn <- dfr - dff

f.val <- ((ss_r - ss_f) / dfn) / (ss_f / dff)
round(f.val, 4)

alpha = .05
f.crit <- qf(1 - alpha, dfn, dff)
round(f.crit, 4)

ifelse(f.val > f.crit, "Reject the Null", "Cannot reject the null")

anova(model3_fit, model2_fit)

partial_f_test(model3_fit, model2_fit)

plot(model3_fit)

# Smoke, Model 4

smoke <- model.matrix(~Smoke, data = data.nutrition)

head(smoke)

model4_data <- data.table(Cholesterol = data.nutrition$Cholesterol, Smoke = data.nutrition$Smoke, SmokeYes = smoke[, 2], Fiber = data.nutrition$Fiber)
model4_data$SmokeFiber <- model4_data$Fiber * model4_data$SmokeYes

model4_fit <- lm(formula = Cholesterol ~ Fiber + Smoke + SmokeFiber, data = model4_data)
summary(model4_fit)

round(coef(model4_fit), 4)

round(summary(model4_fit)$r.squared, 4) * 100

model4_data$pred <- predict(model4_fit)

GainCurvePlot(model4_data, "pred", "Cholesterol", "Cholesterol model")

plot(model4_fit)

# Vitamin, Model 5

vitamin <- model.matrix(~VitaminUse, data = data.nutrition)

head(vitamin)

model5_data <- data.table(Cholesterol = data.nutrition$Cholesterol, Vitamin = data.nutrition$VitaminUse, vitamin[, 2:3], Fiber = data.nutrition$Fiber)
model5_data$VitaminOccFiber <- model5_data$Fiber * model5_data$VitaminUseOccasional
model5_data$VitaminRegFiber <- model5_data$Fiber * model5_data$VitaminUseRegular

model5_fit <- lm(formula = Cholesterol ~ Fiber + VitaminUseOccasional + VitaminUseRegular + VitaminOccFiber + VitaminRegFiber, data = model5_data)
summary(model5_fit)

round(coef(model5_fit), 4)

round(summary(model5_fit)$r.squared, 4) * 100

model5_data$pred <- predict(model5_fit)

GainCurvePlot(model5_data, "pred", "Cholesterol", "Cholesterol model")

plot(model4_fit)

