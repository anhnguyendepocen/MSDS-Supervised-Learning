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

#####################################################################
######################### Computation 1 #############################
#####################################################################

path.work <- "E:/GitHub/MSDS-RegressionAnalysis/data"
path.home <- "D:/Projects/MSDS-RegressionAnalysis/data"

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

data.states <- as.data.table(read.csv(file = "USStates.csv", head = TRUE, sep = ","))

# 2.)

skim(data.states)

stats <- as.data.frame(summarytools::descr(data.states))

formattable(stats, align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "r"),
            list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

data.nondemo <- data.states[, !c('State', 'Region', 'Population')]
data.income <- melt(data.nondemo, id.vars = c('HouseholdIncome'))

ggplot(data.income) +
    geom_jitter(aes(value, HouseholdIncome, colour = variable),) +
    geom_smooth(aes(value, HouseholdIncome, colour = variable), method = lm, se = FALSE) +
    facet_wrap(~variable, scales = "free_x")

# 3.)

getCorTable <- function(cols) {
  # selected features correlation matrix
  sale.cor <- cor(cols, use = "pairwise.complete.obs")[, "HouseholdIncome"]
  sale.cor <- sort(sale.cor, decreasing = T)

  sale.cor <- sale.cor[-1] # remove SalePrice

  tbl.sale.cor <- melt(sale.cor)
  colnames(tbl.sale.cor) <- c("Correlation to Household Income")

  formattable(tbl.sale.cor, align = c("l", "r"),
              list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
  ))
}

getCorTable(data.nondemo)

ggpairs(data.nondemo)

# 4.)

ggplotRegression <- function(fit) {
  p1 <- ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ", signif(summary(fit)$adj.r.squared, 5),
                      "Intercept =", signif(fit$coef[[1]], 5),
                      " Slope =", signif(fit$coef[[2]], 5),
                      " P =", signif(summary(fit)$coef[2, 4], 5)))

  res <- as.data.table(residuals(fit))
  colnames(res) <- c("value")

  p2 <- ggplot(res, aes(value, fill = ..count..)) +
    geom_histogram(breaks = pretty(res$value)) +
    labs(title = "Residuals")

  grid.arrange(p1, p2)
}

# Model #1
m1 <- data.nondemo[, .(College, HouseholdIncome)]

model_1 <- lm(formula = HouseholdIncome ~ College, data = m1)
ggplotRegression(model_1)

anova(model_1)
aov(HouseholdIncome ~ College, m1)

summary(model_1)

# Long-hand LM
model_1_slope <- cor(m1$College, m1$HouseholdIncome) * (sd(m1$HouseholdIncome) / sd(m1$College))
model_1_intercept <- mean(m1$HouseholdIncome) - (model_1_slope * mean(m1$College))

# SSE

m1$Y_Hat <- predict(model_1)
m1$residual <- m1$HouseholdIncome - m1$Y_Hat

sse <- sum(m1$residual ** 2)

# SST

y_bar <- mean(m1$HouseholdIncome)

sst <- sum((m1$HouseholdIncome - y_bar) ** 2)

# SSR

ssr <- sum((m1$Y_Hat - y_bar) ** 2)

(ssr / sst)

stopifnot(sst == (ssr + sse))