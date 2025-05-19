

OLS_results = function(ols.simple, ols.control, ols.ira, rlasso.ira) {

  table <- matrix(0, 3, 4)
  table[1,1:4] <- c(ols.simple[2,1], ols.control[2,1], ols.ira[2,1], rlasso.ira[[1]][1])
  table[2,1:4] <- c(ols.simple[2,2], ols.control[2,2], ols.ira[2,2], rlasso.ira[[1]][2])
  table[3,1:4] <- c(ols.simple[2,3], ols.control[2,3], ols.ira[2,3], rlasso.ira[[1]][3])

  estimates  <- table[1, ]
  std_errors <- table[2, ]
  t_values   <- table[3, ]

# Create a data frame
result_table <- data.frame(
  Method = c("CL", "CRA", "IRA", "IRA w Lasso"),
  Estimates = round(estimates, 4),
  `Standard Errors` = round(std_errors, 4),
  `t-values` = round(t_values, 4)
)


plot_data <- data.frame(
  Method = c("CL", "CRA", "IRA", "IRA w Lasso"),
  Estimate = estimates,
  CI_Lower = estimates - 1.96 * std_errors,
  CI_Upper = estimates + 1.96 * std_errors)


my_plot <- ggplot(plot_data, aes(x = Method, y = Estimate)) +
  geom_point(shape = 8, size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.25, position = position_dodge(0.9)) +
  ylim(c(-0.2, 0.2)) +
  labs(title = "Point Estimates with Confidence Intervals", y = "Estimate")


return(list(result_table = result_table, plot = my_plot))
}
