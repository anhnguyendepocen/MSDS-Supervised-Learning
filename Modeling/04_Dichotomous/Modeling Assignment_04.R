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

#####################################################################
### Variable Transformations

summary(data.wine)

skim(data.wine)

# Transform Label Appeal, standardize label appeal to 1-5

data.wine$LabelAppeal <- data.wine$LabelAppeal + 3

ggplot(data.wine, aes(LabelAppeal, fill = ..count..)) +
  geom_histogram()

# Binary Good/Bad Quality Indicator

data.wine$Quality <- ifelse(data.wine$STARS >= 3, "Good", "Bad")
data.wine$Quality <- as.factor(data.wine$Quality)

# Remove wines withs negative alcohol content, must be data errors

data.wine <- data.wine[Alcohol > 0]

# Further data removal
nrow(data.wine[Alcohol < 0 | Sulphates < 0 | CitricAcid < 0 | FreeSulfurDioxide < 0]) # 118

nrow(data.wine[Sulphates < 0])

nrow(data.wine[Chlorides < 0])

nrow(data.wine[CitricAcid < 0])


# EDA

skim(data.wine)

getDistribution <- function(data, col, lab) {
  p1 <- ggplot(data, aes(x = 1, col)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(alpha = 0.2, color = 'red') +
    labs(y = lab, x = '')

  p2 <- ggplot(data, aes(col, fill = ..count..)) +
    geom_histogram(breaks = pretty(col)) +
    labs(x = lab)

  grid.arrange(p1, p2, nrow = 1)
}

# ggpairs(data.wine)

# Alcohol, Continious

getDistribution(data.wine, data.wine$Alcohol, 'Alcohol')

### Chlorides, Continious

getDistribution(data.wine, data.wine$Chlorides, 'Chlorides')

# CitricAcid, Continuous

getDistribution(data.wine, data.wine$CitricAcid, 'Citric Acid')

# Density, Continious

getDistribution(data.wine, data.wine$Density, 'Density')

# Fixed Acidity

getDistribution(data.wine, data.wine$FixedAcidity, 'Fixed Acidity')

# Free Sulfur Dioxide, Continuous

getDistribution(data.wine, data.wine$FreeSulfurDioxide, 'Free-Sulfur Dioxide')

# pH, Continious

getDistribution(data.wine, data.wine$pH, 'pH')

# ResidualSugar, Continious

getDistribution(data.wine, data.wine$ResidualSugar, 'ResidualSugar')

# Total Sulfur Dioxide, Continious

getDistribution(data.wine, data.wine$TotalSulfurDioxide, 'Total Sulfur Dioxide')

# Sulphates, Continious

getDistribution(data.wine, data.wine$Sulphates, 'Sulphates')

# Volatile Acidity, Continuous

getDistribution(data.wine, data.wine$VolatileAcidity, 'Volatile Acidity')

# Label Appeal, Normal Discrete

ggplot(data.wine, aes(LabelAppeal, fill = ..count..)) +
  geom_histogram()

getDistribution(data.wine, data.wine$LabelAppeal, 'Label Appeal')

# Acid Index, Poisson Discrete
ggplot(data.wine, aes(AcidIndex, fill = ..count..)) +
  geom_histogram()

getDistribution(data.wine, data.wine$AcidIndex, 'Acid Index')

#####################################################################
### Response, STARS

data.stars <- data.wine[!is.na(STARS)]

# Poisson Distribution
ggplot(data.wine, aes(STARS, fill = ..count..)) +
  geom_histogram()

ggplot(data.stars, aes(STARS, Alcohol)) +
  geom_point()

ggplot(data.stars, aes(STARS, Chlorides)) +
  geom_point()

ggplot(data.stars, aes(STARS, CitricAcid)) +
  geom_point()

ggplot(data.stars, aes(STARS, ResidualSugar)) +
  geom_point()

summary(data.stars$Alcohol)

ggplot(data.stars, aes(STARS, Density)) +
  geom_point()

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