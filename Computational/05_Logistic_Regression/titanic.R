data(titanic_train, package = "titanic")
d <- titanic_train # less typing

glm1 <- glm(Survived ~ Pclass, data = d, family = "binomial")

coef(glm1)

logit2prob <- function(logit) {
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob(coef(glm1))

library(ggplot2)


logit_seq <- seq(-10, 10, by = .1)

prob_seq <- logit2prob(logit_seq)

rm(df)

df <- data.frame(Logit = logit_seq,
                 Probability = prob_seq)

ggplot(df) +
  aes(x = logit_seq, y = prob_seq) +
  geom_point(size = 2, alpha = .3) +
  labs(x = "logit", y = "probability of success")
