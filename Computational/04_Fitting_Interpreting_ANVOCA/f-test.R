# Function to Perform Partial F-Tests
partial_f_test <- function(full_mod, partial_mod, alpha = 0.05) {
  # Add ANOVA objects
  full_anova <- anova(full_mod)
  partial_anova <- anova(partial_mod)

  # Calculate sum of squares
  full_regression_ss <-
    sum(full_anova$`Sum Sq`[1:(length(full_anova$`Sum Sq`) - 1)])
  reduced_regression_ss <-
    sum(partial_anova$`Sum Sq`[1:(length(partial_anova$`Sum Sq`) - 1)])

  s <- sum(partial_anova$Df[1:(length(partial_anova$Df) - 1)])
  df <- full_anova$Df[length(full_anova$Df)]

  # F-statistic computations
  full_ms_residual <-
    full_anova$`Sum Sq`[length(full_anova$`Sum Sq`)] / df
  numerator <- (full_regression_ss - reduced_regression_ss) / s
  denominator <- full_ms_residual
  partial_f <- round(numerator / denominator, digits = 4)

  # Critical F
  n <- sum(full_anova$Df) + 1
  q_p <- sum(full_anova$Df[1:length(full_anova$Df) - 1])
  df2 <- n - q_p - 1
  critical_f <- round(qf(1 - alpha, s, df2), digits = 4)

  # Print based on reject or fail to reject
  if (partial_f > critical_f) {
    print(
      paste(
        "The F-statistic is",
        partial_f,
        "which is greater than the critical value of",
        critical_f
      )
    )
    print("Therefore we can REJECT the null hypothesis")
  } else {
    print(
      paste(
        "The F-statistic is",
        partial_f,
        "which is less than the critical value of",
        critical_f
      )
    )
    print("Therefore we FAIL TO REJECT the null hypothesis")
  }
}