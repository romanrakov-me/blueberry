
#' Pseudo-outcomes AIPW estimation
#'
#'@description
#'
#'
#'
#' @param X Feature matrix
#' @param W Binary treatment indicator
#' @param Y Response variable vector

#' @param e Propensity score: true or estimated
#' @param nfolds Numbers of folds for cross-fitting

#'
#' @return Returns a vector of pseudo-outcomes
#' @export
#'
#' @examples Not yet

aipw_GATE <- function(X,W,Y,e,nfolds) {

  nfolds = 2
  n <- length(Y)
  e = 0.35

  fold <- sample(1:nfolds, n, replace = T)

  mwhat0 = mwhat1 = rep(NA, n)

  for (i in 1:nfolds) {

    rfm0 <- regression_forest(X[fold != i & W == 0,], Y[fold != i & W == 0])
    mwhat0[fold == i] <- predict(rfm0, X[fold == i,])$predictions

    rfm1 <- regression_forest(X[fold != i & W == 1,], Y[fold != i & W == 1])
    mwhat1[fold == i] <- predict(rfm1, X[fold == i,])$predictions
  }

  Y_tilde <- mwhat1 - mwhat0 + W * (Y - mwhat1) / e - (1 - W) * (Y - mwhat0)/(1 - e)

    return(Y_tilde)

}
