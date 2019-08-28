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
library(glmulti)

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

alcohol.removed <- nrow(data.wine[Alcohol < 0 | is.na(Alcohol)])

# Remove negative ABV values.
data.wine <- data.wine[Alcohol >= 0]

alcohol.removed / nrow(data.wine)

# Binary Good/Bad Quality Indicator

data.wine$Quality <- ifelse(data.wine$STARS >= 3, "Good", "Bad")
data.wine$Quality <- as.factor(data.wine$Quality)

######################################################################################
### Univariate3 EDA
#####################################################################################

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

normalize <- function(var) {
  (var - mean(var, na.rm = T)) / sd(var, na.rm = T)
}

# Alcohol, Continious

getDistribution(data.wine, data.wine$Alcohol, 'Alcohol')

### Chlorides, Continious

summary(data.wine$Chlorides)

getDistribution(data.wine, data.wine$Chlorides, 'Chlorides')

data.wine$Norm_Chlorides <- scale(data.wine$Chlorides)

getDistribution(data.wine, data.wine$Norm_Chlorides, 'Norm Chlorides')

# CitricAcid, Continuous

summary(data.wine$CitricAcid)

getDistribution(data.wine, data.wine$CitricAcid, 'Citric Acid')

data.wine$Norm_CitricAcid <- scale(data.wine$CitricAcid)

getDistribution(data.wine, data.wine$Norm_CitricAcid, 'Norm Citric Acid')

# Fixed Acidity

summary(data.wine$FixedAcidity)

getDistribution(data.wine, data.wine$FixedAcidity, 'Fixed Acidity')

data.wine$Norm_FixedAcidity <- scale(data.wine$FixedAcidity)

getDistribution(data.wine, data.wine$Norm_FixedAcidity, 'Fixed Acidity')

# Free Sulfur Dioxide, Continuous

summary(data.wine$FreeSulfurDioxide)

getDistribution(data.wine, data.wine$FreeSulfurDioxide, 'Free-Sulfur Dioxide')

data.wine$Norm_FreeSulfurDioxide <- scale(data.wine$FreeSulfurDioxide)

getDistribution(data.wine, data.wine$Norm_FSD, 'Free-Sulfur Dioxide')

# ResidualSugar, Continious

summary(data.wine$ResidualSugar)

getDistribution(data.wine, data.wine$ResidualSugar, 'ResidualSugar')

data.wine$Norm_ResidualSugar <- scale(data.wine$ResidualSugar)

getDistribution(data.wine, data.wine$Norm_ResidualSugar, 'Norm ResidualSugar')

# Total Sulfur Dioxide, Continious

summary(data.wine$TotalSulfurDioxide)

getDistribution(data.wine, data.wine$TotalSulfurDioxide, 'Total Sulfur Dioxide')

data.wine$Norm_TotalSulfurDioxide <- scale(data.wine$TotalSulfurDioxide)

getDistribution(data.wine, data.wine$Norm_TotalSulfurDioxide, 'Norm Total Sulfur Dioxide')

# Sulphates, Continious

summary(data.wine$Sulphates)

getDistribution(data.wine, data.wine$Sulphates, 'Sulphates')

data.wine$Norm_Sulphates <- scale(data.wine$Sulphates)

getDistribution(data.wine, data.wine$Norm_Sulphates, 'Sulphates')

# Volatile Acidity, Continuous

summary(data.wine$VolatileAcidity)

getDistribution(data.wine, data.wine$VolatileAcidity, 'Volatile Acidity')

data.wine$Norm_VolAcidity <- scale(data.wine$VolatileAcidity)

getDistribution(data.wine, data.wine$Norm_VolAcidity, 'Norm Volatile Acidity')

# Density, Continious

getDistribution(data.wine, data.wine$Density, 'Density')

# pH, Continious

ggplot(data.wine, aes(x = pH, fill = Quality)) +
  geom_density(alpha = .3)

getDistribution(data.wine, data.wine$pH, 'pH')

# Label Appeal, Normal Discrete

ggplot(data.wine, aes(LabelAppeal, fill = ..count..)) +
  geom_histogram()

getDistribution(data.wine, data.wine$LabelAppeal, 'Label Appeal')

# Acid Index, Poisson Discrete

ggplot(data.wine, aes(AcidIndex, fill = ..count..)) +
  geom_histogram()

getDistribution(data.wine, data.wine$AcidIndex, 'Acid Index')

######################################################################################
### Model Data Correlations
#####################################################################################

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

skim(data.wine)

model.numeric.col <- unlist(lapply(data.wine, is.numeric))
data.wine.numeric <- data.wine[, model.numeric.col, with = F]

str(data.wine.numeric)

getCorTable(data.wine.numeric)

# Correlation matrix
ggcorrplot(round(cor(data.wine.numeric[, 1:16]), 1),
           type = "lower",
           method = "circle",
           colors = c("tomato2", "white", "springgreen3"),
           lab_size = 3,
           title = "Correlogram of Wine Metrics")

ggplot(data = data.wine,
       aes(y = Density, x = Alcohol,
           color = STARS)) +
           geom_point(alpha = 0.8, size = 1) +
           geom_smooth(method = "lm", se = FALSE, size = 1) +
           scale_color_brewer(type = 'seq',
                   guide = guide_legend(title = 'Quality'))

PairPlot(data.wine,
         colnames(data.wine)[4:10],
         "Chemical Properties by Quality",
         group_var = "Quality")

######################################################################################
### Bivariate EDA
#####################################################################################

data.complete <- data.wine[complete.cases(data.wine)]

ggplot(data.complete, aes(x = CitricAcid, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = Chlorides, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = FixedAcidity, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = FreeSulfurDioxide, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = ResidualSugar, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = TotalSulfurDioxide, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = Sulphates, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = VolatileAcidity, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = Density, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = pH, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = AcidIndex, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = AcidIndex, fill = as.factor(STARS))) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = Alcohol, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = Alcohol, fill = as.factor(STARS))) +
  geom_density(alpha = .3)

# Alcohol ~ Label

ggplot(data.wine, aes(Alcohol, fill = LabelAppeal, colour = LabelAppeal)) +
  geom_density(alpha = 0.1) + facet_grid(LabelAppeal ~ .)

# STARS

ggplot(data.wine, aes(STARS, CitricAcid, group = STARS, fill = STARS)) +
  geom_boxplot(varwidth = T)

ggplot(data.wine, aes(STARS, Chlorides, group = STARS, fill = STARS)) +
  geom_boxplot(varwidth = T)

ggplot(data.wine, aes(STARS, FixedAcidity, group = STARS, fill = STARS)) +
  geom_boxplot(varwidth = T)

ggplot(data.wine, aes(STARS, FreeSulfurDioxide, group = STARS, fill = STARS)) +
  geom_boxplot(varwidth = T)

ggplot(data.wine, aes(STARS, ResidualSugar, group = STARS, fill = STARS)) +
  geom_boxplot(varwidth = T)

ggplot(data.wine, aes(STARS, TotalSulfurDioxide, group = STARS, fill = STARS)) +
  geom_boxplot(varwidth = T)

ggplot(data.wine, aes(STARS, Sulphates, group = STARS, fill = STARS)) +
  geom_boxplot(varwidth = T)

ggplot(data.wine, aes(STARS, VolatileAcidity, group = STARS, fill = STARS)) +
  geom_boxplot(varwidth = T)

ggplot(data.wine, aes(STARS, pH, group = STARS, fill = STARS)) +
  geom_boxplot(varwidth = T)

ggplot(data.wine, aes(STARS, AcidIndex, group = STARS, fill = STARS)) +
  geom_boxplot()

ggplot(data.wine, aes(STARS, Density, group = STARS, fill = STARS)) +
  geom_boxplot()

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

ggplot(data = data.complete, aes(x = Quality, y = FixedAcidity, group = Quality)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

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

ggplot(data = data.wine, aes(x = Cases, y = TotalSulfurDioxide, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = ResidualSugar, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = Sulphates, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = AcidIndex, group = Cases)) +
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

ggplot(data = data.wine, aes(x = Cases, y = Alcohol, group = Cases)) +
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

nrow(data.stars) - nrow(data.wine)

n.total.stars <- nrow(data.stars)

data.stars$u <- runif(n = n.total.stars, min = 0, max = 1)

# Create train/test split;
stars.train <- subset(data.stars, u < 0.70)
stars.test <- subset(data.stars, u >= 0.70)

# Poisson Distribution
ggplot(stars.train, aes(STARS, fill = ..count..)) +
  geom_histogram() +
  labs(title = "STARS Rating")

nrow(stars.train[STARS <= 2]) / nrow(stars.train)

dist <- stars.train[, .(Count = .N / nrow(stars.train)), by = STARS]
dist[, Poisson := dpois(STARS, 1)]

ggplot(dist) +
  geom_line(aes(STARS, Count, color = "Stars"), lwd = 1.5) +
  geom_line(aes(STARS, Poisson, color = "Poisson"), lwd = 1.5, linetype = "dashed") +
  labs(title = "Stars Distribution vs Possion Distribution") +
  theme(legend.position = "bottom")

# STARS EDA

ggplot(data = stars.train, aes(x = STARS, y = FixedAcidity, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = stars.train, aes(x = STARS, y = LabelAppeal, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = stars.train, aes(x = STARS, y = VolatileAcidity, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = stars.train, aes(x = STARS, y = ResidualSugar, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = stars.train, aes(x = STARS, y = TotalSulfurDioxide, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = stars.train, aes(x = STARS, y = pH, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = stars.train, aes(x = STARS, y = CitricAcid, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

skim(stars.train)

stars.numeric.col <- unlist(lapply(stars.train, is.numeric))
stars.wine.numeric <- stars.train[, stars.numeric.col, with = F]

str(stars.wine.numeric)

getCorTable(stars.wine.numeric)

# Correlation matrix
ggcorrplot(round(cor(stars.wine.numeric[, 1:16]), 1),
           type = "lower",
           method = "circle",
           colors = c("tomato2", "white", "springgreen3"),
           lab_size = 3,
           title = "Correlogram of Wine Metrics ~ Stars")


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
