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

ggplot(data.religion, aes(INCOME, RELSCHOL)) +
  geom_point()

ggplot(data.religion, aes(ATTEND, RELSCHOL)) +
  geom_point()

