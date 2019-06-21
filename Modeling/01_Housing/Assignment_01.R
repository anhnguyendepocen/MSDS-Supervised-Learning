library(data.table)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(formattable)
library(scales)

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

# Clean-up

data.housing[, TotalFloorSF := FirstFlrSF + SecondFlrSF]
data.housing[, HouseAge := YrSold - YearBuilt]
data.housing[, QualityIndex := OverallQual * OverallCond]
data.housing[, logSalePrice := log(SalePrice)]
data.housing[, Price_Sqft := SalePrice / TotalFloorSF]

summary(data.housing)

# Data Survey

ggplot(data.housing) +
  geom_histogram(aes(data.housing$Price_Sqft, fill = ..count..), breaks = pretty(data.housing$Price_Sqft)) +
  labs(x = "Price / Sqft", y = "Count")

str(subdat)

subdatnum <- subset(data.housing, select = c("TotalFloorSF", "HouseAge", "QualityIndex",
                                  "SalePrice", "LotArea", "OverallQual", "logSalePrice"))

#####################################################################
######################### Assignment 1 ##############################
#####################################################################


# General EDA

data.types <- data.frame(Type = c('Nominal', 'Ordinal', 'Discrete', 'Continious'), Count = c(23, 23, 14, 20))

unique(data.housing$BldgType)

ggplot(data.types, aes(Type, Count, fill = Count)) +
  geom_bar(stat = 'identity') +
  labs(title = "Data Types for Ames Housing",
    caption = "MSDS 410: Regression Analysis 1") +
        theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))

building.info <- data.housing[, .(Count = .N, Pct = percent((.N / nrow(data.housing))), AvgSalePrice = currency(mean(SalePrice))), by = BldgType]

formattable(building.info, align = c("l", "c", "c", "c", "r"),
            list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

str(building.info)

# Model Data

model.data <- subset(data.housing, select = c("TotalFloorSF", "HouseAge", "QualityIndex",
                                  "Price_Sqft", "SalePrice", "LotArea",
                                  "BsmtFinSF1", "Neighborhood", "HouseStyle",
                                  "LotShape", "OverallQual", "logSalePrice",
                                  "TotalBsmtSF", "HouseStyle", "BldgType"))

data.model <- model.data[BldgType == "1Fam"]

ggplot(data.model, aes(y = data.model$SalePrice)) +
  geom_boxplot(outlier.colour = "black", fill = "#1C93D1", outlier.shape = 16,
             outlier.size = 2, notch = FALSE) +
  coord_flip() +
  labs(x = "", y = "Sale Price") +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2))

sd(data.model$SalePrice) * 3
mean(data.model$SalePrice)

data.model <- data.model[SalePrice < 700000]

ggplot(data.model, aes(y = data.model$SalePrice)) +
  geom_boxplot(outlier.colour = "black", fill = "#1C93D1", outlier.shape = 16,
             outlier.size = 2, notch = FALSE) +
  coord_flip() +
  labs(x = "", y = "Sale Price") +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2))

