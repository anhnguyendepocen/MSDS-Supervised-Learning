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
library(RColorBrewer)
library(ggcorrplot)

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

# Negative Value Removal

skim(data.complete)

data.complete <- data.wine[complete.cases(data.wine)]

skim(data.nonneg)

data.nonneg <- data.complete[Chlorides >= 0]
data.nonneg <- data.nonneg[CitricAcid >= 0]
data.nonneg <- data.nonneg[FixedAcidity >= 0]
data.nonneg <- data.nonneg[FreeSulfurDioxide >= 0]
data.nonneg <- data.nonneg[ResidualSugar >= 0]
data.nonneg <- data.nonneg[TotalSulfurDioxide >= 0]
data.nonneg <- data.nonneg[VolatileAcidity >= 0]
data.nonneg <- data.nonneg[Sulphates >= 0]

# EDA

skim(data.wine)

getDistribution <- function(data, col, lab) {
  p1 <- ggplot(data, aes(x = 1, col)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(alpha = 0.2, color = 'red') +
    labs(y = lab, x = '')

  p2 <- ggplot(data, aes(col, fill = ..count..)) +
    geom_histogram() +
    labs(x = lab)

  grid.arrange(p1, p2, nrow = 1)
}

# ggpairs(data.wine)

# Alcohol, Continious

getDistribution(data.nonneg, data.nonneg$Alcohol, 'Alcohol')

getDistribution(data.wine, data.wine$Alcohol, 'Alcohol')

ggplot(data.wine, aes(x = Alcohol, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.wine, aes(Alcohol, colour = LabelAppeal)) +
  geom_density(alpha = 0.1)

ggplot(data.wine, aes(Alcohol, fill = LabelAppeal, colour = LabelAppeal)) +
  geom_density(alpha = 0.1) + facet_grid(LabelAppeal ~ .)

### Chlorides, Continious

ggplot(data.wine, aes(x = Chlorides, fill = Quality)) +
  geom_density(alpha = .3)

getDistribution(data.wine, data.wine$Chlorides, 'Chlorides')

standardize

# CitricAcid, Continuous

ggplot(data.wine, aes(x = CitricAcid, fill = Quality)) +
  geom_density(alpha = .3)

getDistribution(data.wine, data.wine$CitricAcid, 'Citric Acid')

getDistribution(data.nonneg, log(data.nonneg$CitricAcid), 'Citric Acid')
getDistribution(data.nonneg, data.nonneg$CitricAcid, 'Citric Acid')

# Density, Continious

ggplot(data.wine, aes(x = Density, fill = Quality)) +
  geom_density(alpha = .3)

getDistribution(data.wine, data.wine$Density, 'Density')

getDistribution(data.nonneg, data.nonneg$Density, 'Density')

# Fixed Acidity
ggplot(data.wine, aes(x = FixedAcidity, fill = Quality)) +
  geom_density(alpha = .3)

getDistribution(data.wine, data.wine$FixedAcidity, 'Fixed Acidity')

# Free Sulfur Dioxide, Continuous

ggplot(data.wine, aes(x = FreeSulfurDioxide, fill = Quality)) +
  geom_density(alpha = .3)

getDistribution(data.wine, data.wine$FreeSulfurDioxide, 'Free-Sulfur Dioxide')

getDistribution(data.wine, sqrt(data.wine$FreeSulfurDioxide), 'Free-Sulfur Dioxide')
getDistribution(data.wine, log(data.wine$FreeSulfurDioxide), 'Free-Sulfur Dioxide')
getDistribution(data.wine, log10(data.wine$FreeSulfurDioxide), 'Free-Sulfur Dioxide')

# pH, Continious

ggplot(data.wine, aes(x = pH, fill = Quality)) +
  geom_density(alpha = .3)

getDistribution(data.wine, data.wine$pH, 'pH')

# ResidualSugar, Continious

getDistribution(data.wine, data.wine$ResidualSugar, 'ResidualSugar')

# Total Sulfur Dioxide, Continious

getDistribution(data.wine, data.wine$TotalSulfurDioxide, 'Total Sulfur Dioxide')

# Sulphates, Continious

getDistribution(data.wine, data.wine$Sulphates, 'Sulphates')

# Volatile Acidity, Continuous

getDistribution(data.wine, data.wine$VolatileAcidity, 'Volatile Acidity')

getDistribution(data.nonneg, data.nonneg$VolatileAcidity, 'Volatile Acidity')

getDistribution(data.wine, log(data.wine$CitricAcid), 'Volatile Acidity')

# Label Appeal, Normal Discrete

ggplot(data.wine, aes(LabelAppeal, fill = ..count..)) +
  geom_histogram()

getDistribution(data.wine, data.wine$LabelAppeal, 'Label Appeal')

# Acid Index, Poisson Discrete
ggplot(data.wine, aes(AcidIndex, fill = ..count..)) +
  geom_histogram()

getDistribution(data.wine, data.wine$AcidIndex, 'Acid Index')

# Relationships

ggplot(data.nonneg, aes(STARS, VolatileAcidity, group = STARS, fill = STARS)) +
  geom_boxplot()

ggplot(data.wine, aes(STARS, VolatileAcidity, group = STARS, fill = STARS)) +
  geom_boxplot()

ggplot(data.wine, aes(STARS, TotalSulfurDioxide, group = STARS, fill = STARS)) +
  geom_boxplot()

ggplot(data.nonneg, aes(STARS, TotalSulfurDioxide, group = STARS, fill = STARS)) +
  geom_boxplot()

ggplot(data.wine, aes(STARS, Sulphates, group = STARS, fill = STARS)) +
  geom_boxplot()

ggplot(data.wine, aes(STARS, FixedAcidity, group = STARS, fill = STARS)) +
  geom_boxplot()

ggplot(data.wine, aes(STARS, ResidualSugar, group = STARS, fill = STARS)) +
  geom_boxplot()

ggplot(data.nonneg, aes(STARS, ResidualSugar, group = STARS, fill = STARS)) +
  geom_boxplot()

data.wine[, .(Count = sum(.N)), by = c("STARS")]

getCorTable <- function(cols) {
  # selected features correlation matrix
  sale.cor <- cor(cols, use = "pairwise.complete.obs")[, "STARS"]
  sale.cor <- sort(sale.cor, decreasing = T)

  sale.cor <- sale.cor[-1] # remove SalePrice
  sale.cor <- sale.cor[-1] # remove LogSalePrice

  tbl.sale.cor <- melt(sale.cor)
  colnames(tbl.sale.cor) <- c("Correlation to STARS")

  formattable(tbl.sale.cor, align = c("l", "r"),
              list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
  ))
}

# Model Data Correlations

model.numeric.col <- unlist(lapply(data.wine, is.numeric))
data.wine.numeric <- data.wine[, model.numeric.col, with = F]

str(data.wine.numeric)

getCorTable(data.wine.numeric)

simple_cor_test <- function(x, y) {
  return(cor.test(x, as.numeric(y))$estimate)
}

correlations <- c(
  simple_cor_test(data.wine$FixedAcidity, data.wine$STARS),
  simple_cor_test(data.wine$VolatileAcidity, data.wine$STARS),
  simple_cor_test(data.wine$CitricAcid, data.wine$STARS),
  simple_cor_test(data.wine$ResidualSugar, data.wine$STARS),
  simple_cor_test(data.wine$Chlorides, data.wine$STARS),
  simple_cor_test(data.wine$Sulphates, data.wine$STARS),
  simple_cor_test(data.wine$Density, data.wine$STARS),
  simple_cor_test(data.wine$pH, data.wine$STARS),
  simple_cor_test(data.wine$Alcohol, data.wine$STARS))

names(correlations) <- c('fixed.acidity', 'volatile.acidity', 'citric.acid',
                         'residual.sugar', 'chlordies', 
                         'sulphates', 'density', 
                         'pH', 'alcohol')

correlations

ggplot(data = data.wine,
       aes(y = Density, x = Alcohol,
           color = STARS)) +
           geom_point(alpha = 0.8, size = 1) +
           geom_smooth(method = "lm", se = FALSE, size = 1) +
           scale_color_brewer(type = 'seq',
                   guide = guide_legend(title = 'Quality'))

scatterplotMatrix(data.wine)

# Correlation matrix
corr <- round(cor(data.wine.numeric), 1)

ggcorrplot(round(cor(data.wine.numeric), 1),
           type = "lower",
           method = "circle",
           colors = c("tomato2", "white", "springgreen3"),
           lab_size = 3,
           title = "Correlogram of Wine Metrics")

# Plot
ggcorrplot(corr, hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method = "circle",
           colors = c("tomato2", "white", "springgreen3"),
           title = "Correlogram of mtcars")

# Bivariate Plots

skim(data.wine)

colnames(data.wine)

PairPlot(data.wine,
         colnames(data.wine)[4:10],
         "Chemical Properties by Quality",
         group_var = "Quality")

# Quality

ggplot(data = data.wine, aes(x = Quality, y = FixedAcidity, group = Quality)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Quality, y = VolatileAcidity, group = Quality)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = pH, y = VolatileAcidity, group = Quality)) +
  geom_point()


# STARS

ggplot(data = data.wine, aes(x = STARS, y = FixedAcidity, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = STARS, y = LabelAppeal, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = STARS, y = VolatileAcidity, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = STARS, y = ResidualSugar, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = STARS, y = TotalSulfurDioxide, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = STARS, y = pH, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = STARS, y = CitricAcid, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

# CASES

ggplot(data = data.wine, aes(x = Cases, y = FixedAcidity, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = VolatileAcidity, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = CitricAcid, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = Density, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = pH, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = STARS, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = LabelAppeal, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = LabelAppeal, group = Cases)) +
  geom_jitter() +
  geom_smooth(aes(Cases, LabelAppeal), method = "lm")

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
