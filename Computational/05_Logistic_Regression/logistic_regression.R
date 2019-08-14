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

#####################################################################
######################### Computation 5 #############################
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
data.religion <- as.data.table(read.csv(file = "RELIGION.csv", head = TRUE, sep = ","))

# EDA

attach(data.religion)

str(data.religion)

# Religious School prob / odds
rel.table <- table(RELSCHOL)
rel.prob <- rel.table / nrow(data.religion)
rel.odds <- rel.prob / (1 - rel.prob)

# Q2

Plot(INCOME, RELSCHOL, data = data.religion)

by_income <- data.religion[, .(Prob = sum(RELSCHOL) / .N, Count = .N), by = INCOME]
setorder(by_income, INCOME)
by_income

mean(by_income[INCOME >= 8]$Prob) * 100 # Cut-off point
mean(by_income[INCOME <= 7]$Prob) * 100

sort(unique(data.religion$INCOME))

tbl.income <- table(RELSCHOL, INCOME)
round(prop.table(tbl.income), 3)

p1 <- ggplot(data.religion[, .(Prob = sum(RELSCHOL) / .N), by = INCOME], aes(INCOME, Prob)) +
  geom_point() +
  geom_line() +
  labs( title = "Proportion of Religious School by Income")

p2 <- ggplot(data.religion, aes(INCOME, fill = ..count..)) +
  geom_histogram() +
  labs(title = "Income Distribution")

grid.arrange(p1, p2, nrow = 2)

data.religion$D_INCOME <- ifelse(data.religion$INCOME >= 8, 1, 0)

attach(data.religion)

tbl.d_income <- table(RELSCHOL, D_INCOME)
round(tbl.d_income, 3)

by_attendance <- data.religion[, .(Value = sum(RELSCHOL), Prob = sum(RELSCHOL) / .N, Count = .N), by = ATTEND]
setorder(by_attendance, ATTEND)
by_attendance

tbl.attend <- table(RELSCHOL, ATTEND)
tbl.attend / nrow(data.religion)
round(prop.table(tbl.attend, 1), 3)
round(prop.table(tbl.attend, 2), 3)

ggplot(data.religion, aes(ATTEND, RELSCHOL)) +
  geom_point()

p1 <- ggplot(data.religion[, .(Prob = sum(RELSCHOL) / .N), by = ATTEND], aes(ATTEND, Prob)) +
  geom_point() +
  geom_line() +
  labs(title = "Proportion of Religious School by Attendance")

p2 <- ggplot(data.religion, aes(ATTEND, fill = ..count..)) +
  geom_histogram() +
  labs(title = "Attendence Distribution")

grid.arrange(p1, p2, nrow = 2)

# Q3

# Logistic Regression

summary(data.religion)

log_fit <- function(name, fit) {
  return(data.table(Model = name, AIC = AIC(fit), BIC = BIC(fit)))
}

model1_fit <- glm(RELSCHOL ~ RACE, family = binomial, data = data.religion)
summary(model1_fit)

model1_stats <- log_fit("Model 1", model1_fit)

formattable(model1_stats, align = c("l", "c", "r"),
    list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))
