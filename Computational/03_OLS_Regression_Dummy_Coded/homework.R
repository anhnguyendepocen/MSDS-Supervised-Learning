library(lessR)

path.work <- "E:/GitHub/MSDS-RegressionAnalysis/data"
path.home <- "D:/Projects/MSDS-RegressionAnalysis/data"

if (file.exists(path.home)) {
  setwd(path.home)
} else {
  setwd(path.work)
}

mydata <- read.csv(file = 'HW_Memory_DummyCode.csv', head = TRUE, sep = ",")
colnames(mydata)[1] <- 'Score'

# ANOVA(Score ~ Regime + Memory + Regime * Memory, data = mydata)

fit1 <- lm(Score ~ d_A + d_B + d_C + d_low + d_high + d_Alow + d_Ahigh + d_Blow + d_Bhigh + d_Clow + d_Chigh, data = mydata)
fit1
anova(fit1)
summary(fit1)

fit2 <- lm(Score ~ d_A + d_B + d_C + d_low + d_high, data = mydata)
fit2
anova(fit2)
summary(fit2)