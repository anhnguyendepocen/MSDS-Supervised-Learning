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
#########################   Modeling 4  #############################
#####################################################################

path.w <- "E:/GitHub/MSDS-RegressionAnalysis/data"
path.h <- "D:/Projects/MSDS-RegressionAnalysis/data"

if (file.exists(path.h)) {
  setwd(path.h)
} else {
  setwd(path.w)
}

theme_set(theme_light())

# Theme Overrides
theme_update(plot.title = element_text(hjust = 0.5),
             axis.text.x = element_text(size = 10),
             axis.text.y = element_text(size = 10),
             axis.title = element_text(face = "bold", size = 12, colour = "steelblue4"),
             legend.position = "top", legend.title = element_blank())

data.wine <- as.data.table(read.csv(file = "wine.csv", head = TRUE, sep = ","))
data.wine$`ï..INDEX` <- NULL # remove index column

# EDA

skim(data.wine)

ggpairs(data.wine)

ggplot(data.wine, aes(Cases, fill = ..count..)) +
  geom_histogram()

ggplot(data.wine, aes(Alcohol, fill = ..count..)) +
  geom_histogram()

ggplot(data.wine, aes(Chlorides, fill = ..count..)) +
  geom_histogram()

ggplot(data.wine, aes(ResidualSugar, fill = ..count..)) +
  geom_histogram()

ggplot(data.wine, aes(Cases, fill = ..count..)) +
  geom_histogram()

ggplot(data.wine, aes(STARS, fill = ..count..)) +
  geom_histogram()

# Continuous

ggplot(data.wine, aes(VolatileAcidity, fill = ..count..)) +
  geom_histogram()

# Continuous
ggplot(data.wine, aes(FreeSulfurDioxide, fill = ..count..)) +
  geom_histogram()

# Normal Discrete
ggplot(data.wine, aes(LabelAppeal, fill = ..count..)) +
  geom_histogram()

# Poisson Discrete
ggplot(data.wine, aes(AcidIndex, fill = ..count..)) +
  geom_histogram()

#####################################################################
### Variable Transformations

# Transform Label Appeal, standardize label appeal to 1-5

data.wine$LabelAppeal <- data.wine$LabelAppeal + 3

ggplot(data.wine, aes(LabelAppeal, fill = ..count..)) +
  geom_histogram()

#####################################################################
### Response, STARS

data.stars <- data.wine[!is.na(STARS)]

# Poisson Distribution
ggplot(data.wine, aes(STARS, fill = ..count..)) +
  geom_histogram()

skim(data.stars)

#####################################################################
### Purchase Decision

# Logistic Regression
ggplot(data.wine, aes(Purchase, fill = ..count..)) +
  geom_histogram()


#####################################################################
### Number of Cases Sold

# Zero Inflated Binomial (ZIP)

ggplot(data.wine, aes(Cases, fill = ..count..)) +
  geom_histogram()