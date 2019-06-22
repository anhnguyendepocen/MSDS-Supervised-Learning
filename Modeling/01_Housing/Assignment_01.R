library(data.table)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(formattable)
library(scales)
library(reshape2)

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

#####################################################################
######################### Assignment 1 ##############################
#####################################################################

# Utility Functions

getCorTable <- function(cols) {
  # selected features correlation matrix
  sale.cor <- cor(cols, use = "pairwise.complete.obs")[, "SalePrice"]
  sale.cor <- sort(sale.cor, decreasing = T)

  sale.cor <- sale.cor[-1] # remove SalePrice
  sale.cor <- sale.cor[-1] # remove LogSalePrice

  tbl.sale.cor <- melt(sale.cor)
  colnames(tbl.sale.cor) <- c("Correlation to Sale Price")

  formattable(tbl.sale.cor, align = c("l", "r"),
              list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
  ))
}

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

summary(data.housing)

# Data Survey

ggplot(data.housing) +
  geom_histogram(aes(data.housing$Price_Sqft, fill = ..count..), breaks = pretty(data.housing$Price_Sqft)) +
  labs(x = "Price / Sqft", y = "Count")

housing.numeric.col <- unlist(lapply(data.housing, is.numeric))
data.housing.numeric <- data.housing[, housing.numeric.col, with = F]

str(data.housing.numeric)

getCorTable(data.housing.numeric)

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

data.model <- subset(data.housing, select = c(
                                  # Response
                                  "SalePrice", "logSalePrice",

                                  # Quality
                                  "OverallQual", "QualityIndex",
                                  "Price_Sqft", "Neighborhood",

                                  # Size Related
                                  "TotalFloorSF", "TotalBsmtSF", "FirstFlrSF",
                                  "GrLivArea",

                                  # High Value Features
                                  "GarageCars", "GarageArea", "GarageYrBlt",
                                  "FullBath", "HalfBath", 
                                  "MasVnrArea", "MasVnrType",
                                  "PoolArea", "Fireplaces",
                                  "KitchenQual", "BedroomAbvGr",
                                  "Heating", "Utilities",

                                  # Temporal
                                  "HouseAge", "YearRemodel", "YearBuilt",

                                  # Housing Lot
                                  "LotArea", "LotShape", "LotFrontage",

                                  # Housing Style
                                  "HouseStyle", "BldgType"))

str(data.model)

# Model Data Correlations

model.numeric.col <- unlist(lapply(data.model, is.numeric))
data.model.numeric <- data.model[, model.numeric.col, with = F]

str(data.model.numeric)

getCorTable(data.model.numeric)

# Sample Population

data.model <- data.model[BldgType == "1Fam"]

ggplot(data.model, aes(y = data.model$SalePrice)) +
  geom_boxplot(outlier.colour = "black", fill = "#1C93D1", outlier.shape = 16,
             outlier.size = 2, notch = FALSE) +
  coord_flip() +
  labs(x = "", y = "Sale Price") +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2))

sd(data.model$SalePrice) * 3
mean(data.model$SalePrice)

# Data Quality Checks
data.model <- data.model[SalePrice < 700000]

ggplot(data.model, aes(y = data.model$SalePrice)) +
  geom_boxplot(outlier.colour = "black", fill = "#1C93D1", outlier.shape = 16,
             outlier.size = 2, notch = FALSE) +
  coord_flip() +
  labs(x = "", y = "Sale Price") +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2))