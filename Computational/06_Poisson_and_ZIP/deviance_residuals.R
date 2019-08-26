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

library(AER)
deviance(mod1) / mod1$df.residual
dispersiontest(mod1)

library(car)
influencePlot(mod1)

library(pscl)
mod2 <- zeroinfl(Days ~ Age + Sex, data = quine, dist = "poisson")
AIC(mod1, mod2)

res <- residuals(mod1, type = "deviance")
plot(log(predict(mod1)), res)
abline(h = 0, lty = 2)

qqnorm(res)
qqline(res)