

BLP_lecture = function(tauhat,Y,W,denoise,name){

  # Matrix to store results
  results = matrix(NA,2,2)
  rownames(results) = c("Weight w/","HT w/")
  colnames(results) = c("beta2","S.E.")

  # Create relevant variables
  ehat = mean(W)
  weight = 1 / (ehat * (1-ehat) * rep(1,length(W)))
  wres = W - ehat
  demeaned = tauhat - mean(tauhat)
  interact = wres * demeaned
  H = wres * weight
  HY = H * Y
  Hdenoise = H * denoise

  # Weighting w/ denoising
  results[1,] = summary(lm_robust(Y ~ wres + interact + denoise, weights = weight))$coefficients[3,1:2]

  # HT w/ paper
  results[2,] = summary(lm_robust(HY ~ demeaned + Hdenoise))$coefficients[2,1:2]

  return(results)

}
