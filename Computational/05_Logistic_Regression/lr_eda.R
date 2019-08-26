library(lessR)

path.w <- "E:/GitHub/MSDS-RegressionAnalysis/data"
path.h <- "D:/Projects/MSDS-RegressionAnalysis/data"

if (file.exists(path.h)) {
  setwd(path.h)
} else {
  setwd(path.w)
}


mydata <- read.csv('logit_insurance.csv')
attach(mydata)

head(mydata)

Plot(AGE, TARGET_FLAG, data = mydata)
mytable <- table(TARGET_FLAG, KIDSDRIV)

mytable

prop.table(mytable)

# cell percentages
prop.table(mytable, 1)
# row percentages
prop.table(mytable, 2)
# column percentages

Histogram(AGE, data = mydata)
Histogram(AGE, by1 = TARGET_FLAG, data = mydata)

BoxPlot(AGE, data = mydata)
BoxPlot(AGE, by1 = TARGET_FLAG, data = mydata)

age <- mydata$AGE
age_cat <- ifelse(age <= 20, 2, ifelse(age <= 30, 3, ifelse(age <= 40, 4, ifelse(age <= 50, 5, ifelse(age <= 60, 6, 7)))))
mydata <- data.frame(mydata, age_cat)
mytable <- table(TARGET_FLAG, age_cat)
mytable

prop.table(mytable)

# cell percentages
prop.table(mytable, 1)

# row percentages
prop.table(mytable, 2)

# column percentages
BoxPlot(MVR_PTS, data = mydata)
BoxPlot(MVR_PTS, by1 = TARGET_FLAG, data = mydata)

ttest(x = TARGET_FLAG, y = MVR_PTS)

ANOVA(TARGET_FLAG ~ age_cat, data = mydata)
