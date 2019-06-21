path.work <- "E:/GitHub/MSDS-RegressionAnalysis/data"
path.home <- "D:/Projects/MSDS-RegressionAnalysis/data"

if (file.exists(path.home)) {
  setwd(path.home)
} else {
  setwd(path.work)
}

##################

library(ggplot2)
library(GGally)
theme_set(theme_light())

# Theme Overrides
theme_update(plot.title = element_text(hjust = 0.5),
             axis.text.x = element_text(size = 10),
             axis.text.y = element_text(size = 10),
             axis.title = element_text(face = "bold", size = 12, colour = "steelblue4"),
             legend.position = "top", legend.title = element_blank())

mydata <- read.csv(file = "ames_housing_data.csv", head = TRUE, sep = ",")

ncol(mydata)

str(mydata)
head(mydata)
names(mydata)
mydata$TotalFloorSF <- mydata$FirstFlrSF + mydata$SecondFlrSF
mydata$HouseAge <- mydata$YrSold - mydata$YearBuilt
mydata$QualityIndex <- mydata$OverallQual * mydata$OverallCond
mydata$logSalePrice <- log(mydata$SalePrice)
mydata$price_sqft <- mydata$SalePrice / mydata$TotalFloorSF
summary(mydata$price_sqft)
hist(mydata$price_sqft)
subdat <- subset(mydata, select = c("TotalFloorSF", "HouseAge", "QualityIndex",
                                  "price_sqft", "SalePrice", "LotArea",
                                  "BsmtFinSF1", "Neighborhood", "HouseStyle",
                                  "LotShape", "OverallQual", "logSalePrice",
                                  "TotalBsmtSF", "HouseStyle"))

str(subdat)


subdatnum <- subset(mydata, select = c("TotalFloorSF", "HouseAge", "QualityIndex",
                                  "SalePrice", "LotArea", "OverallQual", "logSalePrice"))

#####################################################################
######################### Assignment 1 ##############################
#####################################################################

# Data Types
head(subdat)

data.types <- data.frame(Type = c('Nominal', 'Ordinal', 'Discrete', 'Continious'), Count = c(23, 23, 14, 20))

ggplot(data.types, aes(Type, Count)) +
  geom_bar(stat = 'identity') +
  labs(title = "Data Types for Ames Housing",
    caption = "MSDS 410: Regression Analysis 1") +
        theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))

# Housing Types

mydata$HouseStyle

ggplot(mydata, aes(HouseStyle)) +
  geom_bar() +
  xlab("Housing Style")

ggplot(mydata, aes(HouseStyle, price_sqft)) +
  geom_point() +
  xlab("House Style")

# Model Variables

ggplot(subdat, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm, se=FALSE)

ggplot(subdat, aes(x=QualityIndex, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 

ggplot(subdat, aes(x = LotShape, y = SalePrice)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Distribution of Sale Price") +
  theme(plot.title = element_text(lineheight = 0.8, face = "bold", hjust = 0.5))

ggplot(subdat, aes(x = HouseStyle, y = SalePrice)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Distribution of Sale Price") +
  theme(plot.title = element_text(lineheight = 0.8, face = "bold", hjust = 0.5))

ggplot(subdat, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Distribution of Sale Price") +
  theme(plot.title = element_text(lineheight = 0.8, face = "bold", hjust = 0.5))

# correlation matrix

nums <- unlist(lapply(mydata, is.numeric))
num_data <- mydata[, nums]

sale.cor <- cor(num_data, use = "pairwise.complete.obs")[, "SalePrice"]
sale.cor <- sort(sale.cor, decreasing = T)

sale.cor <- sale.cor[-1] # remove SalePrice
sale.cor <- sale.cor[-1] # remove LogSalePrice

knitr::kable(head(sale.cor, 10))
