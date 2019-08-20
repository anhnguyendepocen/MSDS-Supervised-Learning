library(lessR)
library(data.table)

path.w <- "E:/GitHub/MSDS-RegressionAnalysis/data"
path.h <- "D:/Projects/MSDS-RegressionAnalysis/data"

if (file.exists(path.h)) {
  setwd(path.h)
} else {
  setwd(path.w)
}

mydata <- as.data.table(read.csv(file = "showerdata.csv", head = TRUE, sep = ","))

Histogram(showerdata = NULL, data = mydata, bin.start = 0, bin.width = 2)

shower <- ifelse(mydata$showerlength > 0, 1, 0)
length <- ifelse(shower == 1, mydata$showerlength, NA)

mydata <- cbind.data.frame(mydata, shower, length)
mydata

Histogram(length = NULL, data = mydata, bin.start = 0, bin.width = 2)
SummaryStats(mydata)


###########  GLM with Poisson Distribution
summary(m1 <- glm(showerlength ~ income + age + sex, family = "poisson", data = mydata))
y_hat1 <- fitted(m1)

y_hat1_hard <- 2.325 + 0.006 * mydata$income + 0.058 * mydata$age - 0.267 * mydata$sex
check <- exp(y_hat1_hard)

summary(m2 <- glm(length ~ income + age + sex, family = "poisson", data = mydata))
y_hat2 <- fitted(m2)

mydata <- cbind.data.frame(mydata, y_hat1, y_hat2)

Histogram(y_hat1 = NULL, data = mydata, bin.start = 0, bin.width = 2)
Histogram(y_hat2 = NULL, data = mydata, bin.start = 0, bin.width = 2)


###########  GLM with Negative Binomial Distribution
install.packages(MASS)
library(MASS)

summary(m3 <- glm.nb(showerlength ~ income + age + sex, data = mydata))
summary(m3 <- glm.nb(showerlength ~ income + age + sex, data = mydata, maxit = 100000))
y_hat3 <- fitted(m3)

summary(m3_l <- glm.nb(length ~ income + age + sex, data = mydata, maxit = 100000))
y_hat3_l <- fitted(m3_l)


Histogram(y_hat3_l)
Histogram(y_hat3)


############   ZIP model
install.packages('pscl')
library(pscl)


summary(m4 <- zeroinfl(showerlength ~ income + age + sex | age, dist = "poisson", data = mydata, EM = TRUE))
y_hat4 <- fitted(m4)
mydata <- cbind.data.frame(mydata, y_hat4)
Histogram(y_hat4 = NULL, data = mydata, bin.start = 0, bin.width = 2)


m5 <- zeroinfl(showerlength ~ sex + income | sex, data = mydata, dist = "negbin", EM = TRUE)
summary(m5)



##########   ZIP model - BY HAND

#############    Use a logistic regression model to fit the shower variable 
#############      Then use a Poisson regression to fit the length variable
#############          Combine the two together to obtain the predicted values


zipfit1 <- glm(shower ~ age + sex + income, family = binomial, data = mydata)
zipfit1
anova(zipfit1)
summary(zipfit1)

y_hatzip1 <- 1.5435 + 0.2757 * mydata$age - 0.7787 * mydata$sex - 0.0051 * mydata$income
zipodds <- exp(y_hatzip1)
prob_hatzip1 <- zipodds / (1 + zipodds)
Histogram(prob_hatzip1)

zipfit2 <- glm(length ~ income + age + sex, family = "poisson", data = mydata)
summary(zipfit2)
y_hatzip2 <- 2.5237 + 0.0070 * mydata$income + 0.0107 * mydata$age - 0.1252 * mydata$sex
pred_hatzip2 <- exp(y_hatzip2)

y_hat6 <- ifelse(prob_hatzip1 < 0.50, 0, pred_hatzip2)

Histogram(y_hat6)

