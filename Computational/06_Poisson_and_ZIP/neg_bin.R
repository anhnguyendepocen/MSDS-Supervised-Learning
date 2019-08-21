# https: / / stats.idre.ucla.edu / r / dae / negative - binomial - regression /

require(foreign)
require(ggplot2)
require(MASS)

dat <- read.dta("https://stats.idre.ucla.edu/stat/stata/dae/nb_data.dta")
dat <- within(dat, {
  prog <- factor(prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))
  id <- factor(id)
})

summary(dat)

ggplot(dat, aes(daysabs, fill = prog)) + geom_histogram(binwidth = 1) + facet_grid(prog ~
    ., margins = TRUE, scales = "free")

with(dat, tapply(daysabs, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

summary(m1 <- glm.nb(daysabs ~ math + prog, data = dat))

m2 <- update(m1, . ~ . - prog)
anova(m1, m2)