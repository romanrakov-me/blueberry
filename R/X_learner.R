
#' X-learner
#'
#'@description
#'Estimates and Predicts X-learner using regression forests.
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
X_learner <- function(X,W,Y,train_idx,e) {

  X_train <- X[train_idx == 1, ]
  X_test  <- X[train_idx == 0, ]

  W_train <- W[train_idx == 1]
  W_test  <- W[train_idx == 0]

  Y_train <- Y[train_idx == 1]
  Y_test  <- Y[train_idx == 0]

  ## For W==1
  # 1] estimate m(0,X) for controls
  tf0 = regression_forest(X_train[W_train==0,], Y_train[W_train==0])

  # 2] using m(0,X), impute unobserved outcomes of treated in case they are "untreated"
  yhat0 = predict(tf0, X_train[W_train==1,])$predictions

  # 3] learn imaginary treatment effect for the treated
  xf1 = regression_forest(X_train[W_train==1,], Y_train[W_train==1]-yhat0)

  # 4] predict treatment effect for the test sample using the model (xf1) trained on W=1
  xf.preds.1 = predict(xf1, X_test)$predictions


  ## For W==0
  # 1] estimate m(1,X) for treated
  tf1 = regression_forest(X_train[W_train==1,], Y_train[W_train==1])

  # 2] using m(1,X), impute unobserved outcomes of control in case they are "treated"
  yhat1 = predict(tf1, X_train[W_train==0,])$predictions

  # 3] learn imaginary treatment effect for the controls
  xf0 = regression_forest(X_train[W_train==0,], yhat1-Y_train[W_train==0])

  # 4] predicts treatment effect for the test sample using the model (xf0) trained on W=0
  xf.preds.0 = predict(xf0, X_test)$predictions


  ## Final treatment effect predictions using X-learner weighted formula
  cate_xl = (1 - e) * xf.preds.1 + e * xf.preds.0

  return(cate_xl)
}

