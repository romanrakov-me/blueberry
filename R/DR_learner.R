
#' DR-learner
#'
#'@description
#'Estimates and Predicts DR-learner using regression forests.
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
DR_learner <- function(X,W,Y,train_idx,e) {

  X_train <- X[train_idx == 1, ]
  X_test  <- X[train_idx == 0, ]

  W_train <- W[train_idx == 1]
  W_test  <- W[train_idx == 0]

  Y_train <- Y[train_idx == 1]
  Y_test  <- Y[train_idx == 0]


  mwhat0 = mwhat1 = rep(NA,length(Y_train))

  ## For W=0:
  # 1] build a model to predict Y with X for controls: m(0,X)
  rfm0 = regression_forest(X_train[W_train==0,],Y_train[W_train==0])

  # 2] use it to  predict m_hat(0,X) for controls (w=0)
  mwhat0[W_train==0] = predict(rfm0)$predictions

  # 3] also, use it to predict m_hat(0,X) for treated (w=1)
  mwhat0[W_train==1] = predict(rfm0, newdata = X_train[W_train==1,])$predictions


  ## For W==1:
  # 1] build a model to predict Y with X for treated: m(1,X)
  rfm1 = regression_forest(X_train[W_train==1,],Y_train[W_train==1])

  # 2] use it to  predict m_hat(1,X) for treated (w=1)
  mwhat1[W_train==1] = predict(rfm1)$predictions

  # 3] also, use it to predict m_hat(1,X) for controls (w=0)
  mwhat1[W_train==0] = predict(rfm1,X_train[W_train==0,])$predictions


  ## Estimate the pseudo outcome:
  Y_tilde = mwhat1 - mwhat0 + W_train * (Y_train - mwhat1) / e - (1 - W_train) * (Y_train - mwhat0) / (1-e)

  ## Flexibly fit Y_tilde with X_train:
  dr_train = regression_forest(X_train,Y_tilde)

  ## Finally, estimate CATE function using test sample:
  cate_dr = predict(dr_train, newdata = X_test)$predictions

  return(cate_dr)
}
