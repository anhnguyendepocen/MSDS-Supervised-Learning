library('MASS')
data("menarche")
m <- glm(cbind(Menarche, Total - Menarche) ~ Age, family = binomial, data = menarche)
summary(m)

#predict gives the predicted value in terms of logits
plot.dat <- data.frame(prob = menarche$Menarche / menarche$Total,
                       age = menarche$Age,
                       fit = predict(m, menarche))
#convert those logit values to probabilities
plot.dat$fit_prob <- exp(plot.dat$fit) / (1 + exp(plot.dat$fit))

library(ggplot2)
ggplot(plot.dat, aes(x = age, y = prob)) +
  geom_point() +
  geom_line(aes(x = age, y = fit_prob))

