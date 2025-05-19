
#' T-learner
#'
#'@description
#'Estimates and Predicts T-learner using regression forests.
#'The estimation is conducted in full-sample.
#'
#'
#' @param X Feature matrix
#' @param W Binary treatment indicator
#' @param Y Response variable vector
#' @param train_idx Training sample indicator:
#' "1" for train,
#' "0" for test
#'
#' @return Returns CATE values estimated on the "train" sample and predicted on the "test" sample
#' @export
#'
#' @examples Not yet
T_learner <- function(X,W,Y,train_idx) {

  X_train <- X[train_idx == 1, ]
  X_test  <- X[train_idx == 0, ]

  W_train <- W[train_idx == 1]
  W_test  <- W[train_idx == 0]

  Y_train <- Y[train_idx == 1]
  Y_test  <- Y[train_idx == 0]


  rfm1 = grf::regression_forest(X_train[W_train==1,],Y_train[W_train==1])
  rfm0 = grf::regression_forest(X_train[W_train==0,],Y_train[W_train==0])

  cate_tl = predict(rfm1, X_test)$predictions - predict(rfm0, X_test)$predictions

  return(cate_tl)
}
