library(MASS)
library(vcd)

data(quine)
fit <- goodfit(quine$Days)
summary(fit)
rootogram(fit)

Ord_plot(quine$Days)

distplot(quine$Days, type = "poisson")

mod1 <- glm(Days ~ Age + Sex, data = quine, family = "poisson")
summary(mod1)
anova(mod1, test = "Chisq")

