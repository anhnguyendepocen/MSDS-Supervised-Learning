library(lessR)

path.w <- "E:/GitHub/MSDS-RegressionAnalysis/data"
path.h <- "D:/Projects/MSDS-RegressionAnalysis/data"

if (file.exists(path.h)) {
  setwd(path.h)
} else {
  setwd(path.w)
}

mydata <- read.csv('Donner Party_LR.csv')

gender <- mydata$Sex - 1
gender

mydata <- data.frame(mydata, gender)
mydata

fitA <- Logit(Survival ~ Age, family = binomial, data = mydata)
fitA

logit1 <- 1.81852 - 0.06647 * mydata$Age
oddsratio1 <- exp(logit1)
pi1 <- exp(logit1) / (1 + exp(logit1))
outcome1 <- ifelse(pi1 > 0.5, 1, 0)

fitB <- lr(Survival ~ Age + gender, family = binomial, data = mydata)
fitB

fit3_lr <- glm(Survival ~ Age, family = binomial, data = mydata)
fit3_lr
anova(fit3_lr)
summary(fit3_lr)

fit4_lr <- glm(Survival ~ Age + Sex, family = binomial, data = mydata)
fit4_lr
anova(fit4_lr)
summary(fit4_lr)

