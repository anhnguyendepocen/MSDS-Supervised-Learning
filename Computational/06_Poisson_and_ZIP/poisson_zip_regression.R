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
library(caret)
library(sjPlot)
library(sjmisc)
library(car)
library(WVPlots)
library(lessR)
library(MASS)

#####################################################################
######################### Computation 6 #############################
#####################################################################

path.w <- "E:/GitHub/MSDS-RegressionAnalysis/data"
path.h <- "D:/Projects/MSDS-RegressionAnalysis/data"

if (file.exists(path.h)) {
  setwd(path.h)
} else {
  setwd(path.w)
}

theme_set(theme_sjplot())

# Theme Overrides
theme_update(plot.title = element_text(hjust = 0.5),
             axis.text.x = element_text(size = 10),
             axis.text.y = element_text(size = 10),
             axis.title = element_text(face = "bold", size = 12, colour = "steelblue4"),
             legend.position = "top", legend.title = element_blank())

# Data set of interest
data.stress <- as.data.table(read.csv(file = "STRESS.csv", head = TRUE, sep = ","))

# Stress Variable

# Q1

summary(data.stress$STRESS)

ggplot(data.stress) +
  geom_histogram(aes(STRESS, fill = ..count..)) +
  labs(title = "Stress Distribution")

ggplot(data.stress, aes(sample = STRESS)) +
  stat_qq() +
  stat_qq_line()

p1 <- ggplot(data.stress) +
  geom_histogram(aes(STRESS, fill = ..count..)) +
  labs(title = "Stress Distribution vs Possion Distribution")

p2 <- ggplot(data.frame(x = c(0:10)), aes(x)) +
    geom_line(aes(y = dpois(x, 1)), colour = "red", lwd = 2)

grid.arrange(p1, p2, nrow = 2)

dist <- data.stress[, .(Count = .N / nrow(data.stress)), by = STRESS]
dist[, Poisson := dpois(STRESS, 1)]

ggplot(dist) +
  geom_line(aes(STRESS, Count, color = "Stress"), lwd = 1.5) +
  geom_line(aes(STRESS, Poisson, color = "Poisson"), lwd = 1.5, linetype = "dashed") +
  labs(title = "Stress Distribution vs Possion Distribution") +
  theme(legend.position = "bottom")

# Q2

model1_fit <- lm(formula = "STRESS ~ COHES + ESTEEM + GRADES + SATTACH", data = data.stress)
summary(model1_fit)

model1_data <- data.stress
model1_data$pred <- predict(model1_fit)

p1 <- ggplot(model1_data, aes(pred, fill = ..count..)) +
  geom_histogram(breaks = pretty(model1_data$pred)) +
  labs(title = "Histogram of Residuals")

p2 <- ggplot(model1_data, aes(sample = pred)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Residual Distribution")

grid.arrange(p1, p2, nrow = 2)

ggplot(model1_data, aes(STRESS, pred)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs( title = "Predicted Stress vs Actual")

mean(data.stress$STRESS)

# Q3

model2_data <- data.stress[, .(STRESS, COHES, ESTEEM, GRADES, SATTACH)]
model2_data$STRESS <- model2_data$STRESS + 0.001 # remove zeros to enable log transformation
model2_data$logSTRESS <- log(model2_data$STRESS)

model2_fit <- lm(formula = "logSTRESS ~ COHES + ESTEEM + GRADES + SATTACH", data = model2_data)

summary(model2_fit)

model2_data$pred <- predict(model2_fit)

p1 <- ggplot(model2_data, aes(pred, fill = ..count..)) +
  geom_histogram(breaks = pretty(model1_data$pred)) +
  labs(title = "Histogram of Residuals")

p2 <- ggplot(model2_data, aes(sample = pred)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Residual Distribution")

grid.arrange(p1, p2, nrow = 2)

ggplot(model2_data, aes(STRESS, pred)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs(title = "Predicted Stress vs Actual")

# 4.)

model3_data <- data.stress[, .(STRESS, COHES, ESTEEM, GRADES, SATTACH)]
model3_fit <- glm(formula = "STRESS ~ COHES + ESTEEM + GRADES + SATTACH", family = poisson, data = model3_data)
summary(model3_fit)

model3_data$pred <- predict(model3_fit)

p1 <- ggplot(model3_data, aes(pred, fill = ..count..)) +
  geom_histogram(breaks = pretty(model1_data$pred)) +
  labs(title = "Histogram of Residuals")

p2 <- ggplot(model3_data, aes(sample = pred)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Residual Distribution")

grid.arrange(p1, p2, nrow = 2)

b0 <- coef(model3_fit)[1]
b0_exp <- exp(b0)

summary(model3_data$STRESS)

p1 <- ggplot(data.stress, aes(STRESS, fill = ..count..)) +
  geom_histogram()

p2 <- ggplot(data.stress, aes(COHES, fill = ..count..)) +
  geom_histogram()

p3 <- ggplot(data.stress, aes(ESTEEM, fill = ..count..)) +
  geom_histogram()

p4 <- ggplot(data.stress, aes(SATTACH, fill = ..count..)) +
  geom_histogram()

grid.arrange(p1, p2, p3, p4, nrow = 2)

b1 <- coef(model3_fit)[2]
b1_exp <- round(exp(b1), 3)
1 - b1_exp

b2 <- coef(model3_fit)[3]
b2_exp <- round(exp(b2), 3)
1 - b2_exp

b3 <- coef(model3_fit)[4]
b3_exp <- round(exp(b3), 3)
1 - b3_exp

b4 <- coef(model3_fit)[5]
b4_exp <- round(exp(b4), 3)
1 - b4_exp


model4_data <- data.stress[, .(STRESS, COHES, ESTEEM, GRADES, SATTACH)]
summary(model4_fit <- glm.nb(STRESS ~ COHES + ESTEEM + GRADES + SATTACH, data = model4_data, maxit = 100000))

round(coef(model4_fit), 3)

AIC(model3_fit) - AIC(model4_fit)

# 5

values <- data.stress$COHES
val.mean <- mean(values)
val.sd <- sd(values)

dt.cutoff <- data.table(Low = val.mean - val.sd, Medium = val.mean + val.sd)

model5_data <- data.stress
model5_data$Group <- ifelse(model5_data$COHES > dt.cutoff$Medium, 3, ifelse(model5_data$COHES < dt.cutoff$Low, 1, 2))

# Verify Cut-points
ggplot(model5_data, aes(COHES, Group)) +
  geom_point()

b2_val <- mean(data.stress$ESTEEM)
b3_val <- mean(data.stress$GRADES)
b4_val <- mean(data.stress$SATTACH)

# Low

low <- round(exp(b0 + b1 * dt.cutoff$Low + b2 * b2_val + b3 * b3_val + b4 * b4_val), 3)

high <- round(exp(b0 + b1 * dt.cutoff$Medium + b2 * b2_val + b3 * b3_val + b4 * b4_val), 3)

(high - low) / low

ggplot(low.cohes, aes(STRESS, pred)) +
  geom_point()

# 6

