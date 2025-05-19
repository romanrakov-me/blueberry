


#' Monte Carlo draws for coverage
#'
#'@description
#'This function is a bit challenging. It also takes a long time to run. I use it to analyse how the results depend on seed number as well as train/test shares.
#'
#' @param X Feature matrix
#' @param W Binary treatment indicator
#' @param Y Response variable vector
#' @param e Propensity score: true or estimated
#' @param iterations Number of times to rerun the analysis
#' @param train_share Share of the training sample relative to the full data
#'
#' @return Returns shares of confidence intervals of estimated HTE b2 with does NOT cover zero.
#' @export
#'
#' @examples Not yet
#'
#'
monte_carlo = function(X,W,Y,iterations,train_share) {

covers_zero <- matrix(0, nrow = iterations, ncol = 10)

# [!] propensity score changes if (1) one arm or (2) all arms:
e = mean(W)
# e = rep(mean(W), length(Y))


# Outer loop for different seeds
for (seed in 1:iterations) {

  set.seed(120+seed)

  indices <- sample(1:nrow(X), size = train_share * nrow(X))

  train_idx <- numeric(nrow(Penn))
  train_idx[indices]  <- 1


  cate_tl    = T_learner(X, W, Y, train_idx)
  cate_xl    = X_learner(X, W, Y, train_idx, e)
  cf         = causal_forest(X[indices,], Y[indices], W[indices], W.hat = e)
  cate_cf    = predict(cf,X[-indices,])$predictions
  cate_dr    = DR_learner(X, W, Y, train_idx, e)
  cate_rl_rf = R_learner(X, W, Y, train_idx, e)

  # De-noising
  rfm0     = regression_forest(X[indices[W[indices] == 0], ],Y[indices[W[indices] == 0]])
  mhat0_rf = predict(rfm0, X[-indices,])$predictions


  BLP_tl = BLP_lecture(cate_tl, Y[-indices], W[-indices], mhat0_rf, "T-learner RF")
  BLP_xl = BLP_lecture(cate_xl,Y[-indices], W[-indices], mhat0_rf, "X-learner RF")
  BLP_cf = BLP_lecture(cate_cf, Y[-indices], W[-indices], mhat0_rf, "Causal Forest")
  BLP_dr = BLP_lecture(cate_dr, Y[-indices], W[-indices], mhat0_rf, "DR-learner")
  BLP_rl = BLP_lecture(cate_rl_rf, Y[-indices], W[-indices], mhat0_rf, "R-learner")

  result_list = list(BLP_tl, BLP_xl, BLP_cf, BLP_dr, BLP_rl)

  results_df_list = lapply(seq_along(result_list), function(i) {
    results_df = as.data.frame(result_list[[i]])
    return(results_df)})

  combined_results_df = do.call(rbind, results_df_list)

  # Calculate lower and upper bounds for confidence intervals
  combined_results_df$lower = combined_results_df$beta2 - 1.96 * combined_results_df$S.E.
  combined_results_df$upper = combined_results_df$beta2 + 1.96 * combined_results_df$S.E.

  # Store results in covers_zero matrix
  covers_zero[seed,] <- ifelse(combined_results_df[, 3] <= 0 & combined_results_df[, 4] >= 0, 0, 1)
}

  # Save the final matrix
  simulation_matrix <- data.frame(
  Method = c("T-learner Weight", "T-learner HT","X-learner Weight","X-learner HT","Causal Forest Weight","Causal forest HT","DR-learner Weight","DR-learner HT","R-learner Weight","R-learner HT"),
  Frequency = round(colMeans(covers_zero), 4))

return(simulation_matrix)


}
