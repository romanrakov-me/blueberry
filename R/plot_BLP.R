

plot_BLP = function(result_list, names_list) {

  # Combine results and names into a single data frame
  results_df_list = lapply(seq_along(result_list), function(i) {
    results_df = as.data.frame(result_list[[i]])
    results_df$rowname = rownames(result_list[[i]])
    results_df$model = names_list[i]
    return(results_df)
  })

  combined_results_df = do.call(rbind, results_df_list)

  # Calculate lower and upper bounds for confidence intervals
  combined_results_df$lower = combined_results_df$beta2 - 1.96 * combined_results_df$S.E.
  combined_results_df$upper = combined_results_df$beta2 + 1.96 * combined_results_df$S.E.

  # Plot coefficients and confidence intervals
  ggplot(combined_results_df, aes(x = interaction(rowname, model), y = beta2, ymin = lower, ymax = upper, color = model)) +
    geom_pointrange() +
    xlab("Coefficient Name") +
    ylab("Coefficient Value") +
    ggtitle("Best Linear Predictor (beta2) for Meta-learners:") +
    geom_hline(yintercept = c(0,1), linetype = c("solid","dashed")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
