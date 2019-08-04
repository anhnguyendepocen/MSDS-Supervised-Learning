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
library(lessR)
library(ggiraphExtra)
library(olsrr)
library(caret)
library(sjPlot)
library(sjmisc)
library(car)
library(WVPlots)
library(MASS)

#####################################################################
######################### Assignment 3 ##############################
#####################################################################

path.work <- "E:/GitHub/MSDS-RegressionAnalysis/data"
path.home <- "D:/Projects/MSDS-RegressionAnalysis/data"

if (file.exists(path.home)) {
  setwd(path.home)
} else {
  setwd(path.work)
}

theme_set(theme_light())

# Theme Overrides
theme_update(plot.title = element_text(hjust = 0.5),
             axis.text.x = element_text(size = 10),
             axis.text.y = element_text(size = 10),
             axis.title = element_text(face = "bold", size = 12, colour = "steelblue4"),
             legend.position = "top", legend.title = element_blank())

data.housing <- as.data.table(read.csv(file = "ames_housing_data.csv", head = TRUE, sep = ","))

# Basic Data Structure

ncol(data.housing)
head(data.housing)
names(data.housing)

# New Features

data.housing[, TotalFloorSF := FirstFlrSF + SecondFlrSF]
data.housing[, HouseAge := YrSold - YearBuilt]
data.housing[, QualityIndex := OverallQual * OverallCond]
data.housing[, logSalePrice := log(SalePrice)]
data.housing[, Price_Sqft := SalePrice / TotalFloorSF]
data.housing[, TotalBath := FullBath + HalfBath]

summary(data.housing)

# Data Survey

ggplot(data.housing) +
  geom_histogram(aes(data.housing$Price_Sqft, fill = ..count..), breaks = pretty(data.housing$Price_Sqft)) +
  labs(x = "Price / Sqft", y = "Count")

housing.numeric.col <- unlist(lapply(data.housing, is.numeric))
data.housing.numeric <- data.housing[, housing.numeric.col, with = F]

str(data.housing.numeric)

# getCorTable(data.housing.numeric)

skim(data.housing)

# Data Exploration

ggplot(data.housing) +
  geom_boxplot(aes(x = SaleType, y = SalePrice, fill = Neighborhood)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

ggplot(data.housing) +
  geom_boxplot(aes(x = Zoning, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

ggplot(data.housing) +
  geom_histogram(aes(data.housing$MasVnrArea, fill = ..count..), breaks = pretty(data.housing$MasVnrArea)) +
  labs(x = "Price / Sqft", y = "Count")

summary(data.housing$MasVnrArea)

missing.masvnr <- data.housing[is.na(data.housing$MasVnrArea)]

ggplot(missing.masvnr) +
  geom_histogram(aes(missing.masvnr$SalePrice, fill = ..count..), breaks = pretty(missing.masvnr$SalePrice)) +
  labs(x = "Price / Sqft", y = "Count") +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  scale_x_continuous(labels = dollar_format(largest_with_cents = .2))


# Drop Conditions

data.cleaned <- data.housing
data.clean.stats <- data.table(Step = "Baseline", Records = nrow(data.cleaned))

ggplot(data.cleaned) +
  geom_boxplot(aes(x = BldgType, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

data.cleaned <- data.housing[BldgType == "1Fam"]
data.clean.stats <- rbind(data.clean.stats, data.table(Step = "BldgType", Records = nrow(data.cleaned)))

# Zoning

data.cleaned <- data.cleaned[!(Zoning %in% c("A (agr)", "I (all)", "C (all)"))]
data.clean.stats <- rbind(data.clean.stats, data.table(Step = "Zoning", Records = nrow(data.cleaned)))

unique(data.cleaned$Zoning)

ggplot(data.cleaned) +
  geom_boxplot(aes(x = Zoning, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

# Sale Condition

skim(data.cleaned)

ggplot(data.cleaned) +
  geom_boxplot(aes(x = SaleCondition, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

unique(data.cleaned$SaleCondition)

data.cleaned <- data.cleaned[!(SaleCondition %in% c("Abnorml"))]
data.clean.stats <- rbind(data.clean.stats, data.table(Step = "Sale Condition", Records = nrow(data.cleaned)))

ggplot(data.cleaned) +
  geom_boxplot(aes(x = LotShape, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

ggplot(data.cleaned) +
  geom_boxplot(aes(x = BsmtCond, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

ggplot(data.cleaned) +
  geom_boxplot(aes(x = HouseStyle, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

ggplot(data.cleaned) +
  geom_boxplot(aes(x = TotRmsAbvGrd, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

ggplot(data.cleaned) +
  geom_boxplot(aes(x = TotRmsAbvGrd, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

ggplot(data.cleaned) +
  geom_boxplot(aes(x = RoofStyle, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

ggplot(data.cleaned) +
  geom_boxplot(aes(x = CentralAir, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

# Sale Price (outliers)

summary(data.cleaned$SalePrice)

ggplot(data.cleaned, aes(SalePrice, fill = ..count..)) +
  geom_histogram(breaks = pretty(data.cleaned$SalePrice)) +
  labs(x = "SalePrice", y = "Count") +
  scale_x_continuous(labels = dollar_format(largest_with_cents = .2))

ggplot(data.cleaned, aes(y = SalePrice)) +
  geom_boxplot(outlier.colour = "red", fill = "#1C93D1", outlier.shape = 16,
             outlier.size = 2, notch = FALSE) +
             coord_flip() +
             labs(x = "", y = "Sale Price") +
             scale_y_continuous(labels = dollar_format(largest_with_cents = .2))

data.model <- data.cleaned[data.cleaned$SalePrice < 700000]

ggplot(data.model, aes(y = SalePrice)) +
  geom_boxplot(outlier.colour = "red", fill = "#1C93D1", outlier.shape = 16,
             outlier.size = 2, notch = FALSE) +
             coord_flip() +
             labs(x = "", y = "Sale Price") +
             scale_y_continuous(labels = dollar_format(largest_with_cents = .2))

# Drop Waterfall

data.clean.stats$Step <- factor(data.clean.stats$Step, levels = data.clean.stats$Step)
data.clean.stats$id <- seq_along(data.clean.stats$Records)

ggplot(data.clean.stats, aes(x = id, y = Records, fill = Step)) +
  geom_rect(aes(xmin = id - 0.45, xmax = id + .45, ymin = 0, ymax = Records)) +
  theme(legend.position = "bottom")

# Categorical Variables

# Get all factor columns in the data
catCols <- names(data.model)[sapply(data.model, is.factor)]

results <- data.table( Column = character(), RSq = numeric(), RSE = numeric(), Levels = numeric())

for (col in catCols) {

  tryCatch({
    fmla <- as.formula(paste0("SalePrice ~ ", col))
    fit <- lm(fmla, data.model)
    
    ret <- data.table(Column = col, RSq = summary(fit)$r.squared, RSE = sd(residuals(fit)), Levels = length(coef(fit)))
  
    results <- rbind(results, ret, use.names = T)
  }, error = function(e) {
    print(e)
  })
}

setorder(results, -RSq,RSE)
results

formattable(results, align = c("l", "c", "r"),
  list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

category_model <- function(fmla, data) {
  fmla <- as.formula(fmla)
  category <- dummyVars(fmla, data = data)

  model_fit <- lm(category, data = data)
  
  print(summary(model_fit))
  print(Anova(model_fit, type = "II"))

  plot_model(model_fit, type = "pred")
}

category_model("SalePrice ~ MiscFeature", data.model)
tapply(data.model$SalePrice, data.model$MiscFeature, mean)

category_model("SalePrice ~ Neighborhood", data.model)
tapply(data.model$SalePrice, data.model$Neighborhood, mean)

category_model("SalePrice ~ Fence", data.model)
category_model("SalePrice ~ BsmtQual", data.model)
category_model("SalePrice ~ PoolQC", data.model)
category_model("SalePrice ~ KitchenQual", data.model)
category_model("SalePrice ~ ExterQual", data.model)
category_model("SalePrice ~ Foundation", data.model)

