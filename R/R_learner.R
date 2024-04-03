
#' R-learner
#'
#'@description
#'Estimates and Predicts R-learner using regression forests.
#'The estimation is conducted in full-sample.
#'
#'
#' @param X Feature matrix
#' @param W Binary treatment indicator
#' @param Y Response variable vector
#' @param train_idx Training sample indicator:
#' "1" for train,
#' "0" for test
#' @param e Propensity score: true or estimated
#'
#' @return Returns CATE values estimated on the "train" sample and predicted on the "test" sample
#' @export
#'
#' @examples Not yet

R_learner <- function(X,W,Y,train_idx,e) {

  X_train <- X[train_idx == 1, ]
  X_test  <- X[train_idx == 0, ]

  W_train <- W[train_idx == 1]
  W_test  <- W[train_idx == 0]

  Y_train <- Y[train_idx == 1]
  Y_test  <- Y[train_idx == 0]


  # 1] Get nuisance parameters
  mhat_est = regression_forest(X_train,Y_train)
  mhat     = predict(mhat_est)$predictions
  # e = e_rct [taken directly from the input]

  # 2] Get residuals
  res_y = Y_train - mhat
  res_w = W_train - e

  # Create pseudo-outcome (outcome res divided by treatment res)
  pseudo_rl = res_y / res_w

  # Create weights
  weights_rl = res_w^2

  # Weighted regression with RF
  rrf_fit = regression_forest(X_train, pseudo_rl, sample.weights = weights_rl)
  cate_rl_rf = predict(rrf_fit, newdata = X_test)$predictions

  return(cate_rl_rf)
}
