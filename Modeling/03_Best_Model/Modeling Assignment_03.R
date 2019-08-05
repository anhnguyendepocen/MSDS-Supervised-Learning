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
######################### Modeling 3 ################################
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

# Utility Function
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


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

housing.numeric.col <- unlist(lapply(data.housing, is.numeric))
data.housing.numeric <- data.housing[, housing.numeric.col, with = F]

str(data.housing.numeric)
skim(data.housing)

# Drop Conditions

data.cleaned <- data.housing
data.clean.stats <- data.table(Step = "Baseline", Records = nrow(data.cleaned))

data.cleaned <- data.housing[BldgType == "1Fam"]
data.clean.stats <- rbind(data.clean.stats, data.table(Step = "BldgType", Records = nrow(data.cleaned)))

# Zoning

data.cleaned <- data.cleaned[!(Zoning %in% c("A (agr)", "I (all)", "C (all)"))]
data.clean.stats <- rbind(data.clean.stats, data.table(Step = "Zoning", Records = nrow(data.cleaned)))

# Sale Condition

data.cleaned <- data.cleaned[!(SaleCondition %in% c("Abnorml"))]
data.clean.stats <- rbind(data.clean.stats, data.table(Step = "Sale Condition", Records = nrow(data.cleaned)))

data.model <- data.cleaned[data.cleaned$SalePrice < 700000]

# Drop Waterfall

data.clean.stats$Step <- factor(data.clean.stats$Step, levels = data.clean.stats$Step)
data.clean.stats$id <- seq_along(data.clean.stats$Records)

# Categorical Variables

# Get all factor columns in the data

getCategoryRelationships <- function(data, response) {

  catCols <- names(data)[sapply(data, is.factor)]
  print(length(catCols))

  results <- data.table( Column = character(), RSq = numeric(), RSE = numeric(), MeanDiff = numeric(), Levels = numeric(), PctPopulated = numeric())

  for (col in catCols) {

    tryCatch({
      fmla <- as.formula(paste0(response, " ~ ", col))
      fit <- lm(fmla, data)
      pct_value <- round((1 - sum(is.na(data[[col]])) / nrow(data)) * 100, 1)
      vals <- data.table(value = tapply(data.model[[response]], data.model[[col]], mean))
      mean.diff <- mean(vals[!is.na(value)]$value)

      ret <- data.table(Column = col, RSq = round(summary(fit)$r.squared * 100, 2), RSE = dollar(sd(residuals(fit))), MeanDiff = dollar(mean.diff), Levels = length(coef(fit)), PctPopulated = pct_value)
  
      results <- rbind(results, ret, use.names = T)
    }, error = function(e) {
      print(e)
    })
  }

  setorder(results, - RSq, RSE, PctPopulated, MeanDiff)
  results
}

cat.relationships <- getCategoryRelationships(data.model, "SalePrice")

formattable(cat.relationships, align = c("l", "c", "c", "c", "c", "r"),
  list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

category_model <- function(col, data, response) {

  data <- data.model[, SalePrice, by = c(col)]
  print(summary(data))

  fmla <- as.formula(paste0(response, " ~ ", col))
  category <- dummyVars(fmla, data = data)

  model_fit <- lm(category, data = data)
  
  print(summary(model_fit))
  print(Anova(model_fit, type = "II"))

  plot_model(model_fit, type = "pred")
}

# Neighborhood

category_model("Neighborhood", data.model, "SalePrice")

neighborhood.mean <- melt(tapply(data.model$SalePrice, data.model$Neighborhood, mean))
colnames(neighborhood.mean) <- c("Neighborhood", "MeanPrice")
neighborhood.mean <- neighborhood.mean[!is.na(neighborhood.mean$MeanPrice),]

neighborhood.mean$MeanPrice <- dollar(neighborhood.mean$MeanPrice)

formattable(neighborhood.mean, align = c("l", "r"),
  list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

ggplot(data.model) +
  geom_boxplot(aes(x = Neighborhood, y = SalePrice, fill = Neighborhood)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

# Basement Quality

ggplot(data.model) +
  geom_boxplot(aes(x = BsmtQual, y = SalePrice, fill = BsmtQual)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

category_model("BsmtQual", data.model, "SalePrice")

# Kitchen Qual
category_model("KitchenQual", data.model, "SalePrice")

ggplot(data.model) +
  geom_boxplot(aes(x = KitchenQual, y = SalePrice, fill = KitchenQual)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

# Exterior Qual
category_model("ExterQual", data.model, "SalePrice")

ggplot(data.model) +
  geom_boxplot(aes(x = ExterQual, y = SalePrice, fill = ExterQual)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

# Fence
category_model("Fence", data.model, "SalePrice")

category_model("PoolQC", data.model, "SalePrice")
category_model("ExterQual", data.model, "SalePrice")
category_model("Foundation", data.model, "SalePrice")


category_model("HeatingQC", data.model, "SalePrice")
category_model("GarageType", data.model, "SalePrice")

