library(data.table)

wgt <- c(64, 71, 53, 67, 55, 58, 77, 57, 56, 51, 76, 68)
hgt <- c(57, 59, 49, 62, 51, 50, 55, 48, 42, 42, 61, 57)
age <- c(8, 10, 6, 11, 8, 7, 10, 9, 10, 6, 12, 9)

children <- data.table(WGT = wgt, HGT = hgt, AGE = age, AgeSq = age^2)

model_1 <- lm(formula = WGT ~ HGT, data = children)
summary(model_1)
anova(model_1)

model_2 <- lm(formula = WGT ~ HGT + AGE, data = children)
summary(model_2)
anova(model_2)

model_3 <- lm(formula = WGT ~ HGT + AGE + AgeSq, data = children)
summary(model_3)
anova(model_3)

T <- round(-0.0417 / 0.4224, 2)
T ^ 2

abs(T) > abs(qt(0.01 / 2, 12 - 3 - 2)) # 99% confidence, 2 sided

n <- 12
p <- 3

ssr <- 588.92 + 103.90 + 0.24
sse <- 195.19
sst <- ssr + sse

((sst - sse) / p ) / (sse / (n - p - 1))

alpha1 <- 0.01
beta1 <- 2.186

T <- beta1 / 0.4104
T <- round(T, 4)

T ^ 2

c(beta1 - alpha1, beta1 + alpha1)

crit.value <- abs(qt(alpha1 / 2, 72 - 2)) # 99% confidence, 2 sided
round(crit.value, 4)

abs(T) > crit.value


set.seed(133)
x <- rnorm(20000)
y <- rnorm(20000)
data <- data.frame(x, y)

fit <- lm(x ~ y, data = data)
summary(fit)
anova(fit)

RSS0 <- sum((x - mean(x)) ^ 2) #20181.97, this is same as TSS really
RSS <- sum(fit$residuals ^ 2) #20181.64
p <- 1 #predictors whos coefficient we are testing.
n <- length(y) #number of observations

F <- ((RSS0 - RSS) / p) / (RSS / (n - p - 1))
F
