#' Perform ridge regression using ordinary least squares as well as QR decomposition
#'
#' @import stats
#' 
#' @param formula an object, that is symbolic description of the model to be fitted.
#' @param data a data frame.
#' @param lambda the lambda of the data set(hyperparameter). Default is 0.
#'
#' @return An object of class ridgereg
#'
#' @example ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 1)
#'
#' @export
#' 

ridgereg = function(formula,data,lambda = 0){
  call=match.call()
  x = model.matrix(formula,data)
  y=data[[all.vars(formula)[1]]]
  
  #normalizing x
  for(i in 2:ncol(x)){
    x[,i] = ((x[,i]-mean(x[,i]))/ (sqrt(var((x[,i])))))
  }
  
  #computation using least squares 
  
  beta_r = solve(((t(x) %*% x) + (lambda * diag(ncol(x))))) %*% (t(x) %*% y)
  y_hat = x %*% beta_r
  
  #computation using QR
  y_qr = as.matrix(data[,all.vars(formula)[1]])
  qr = qr(x)
  R = qr.R(qr)
  I = diag(lambda, nrow = ncol(x))
  beta_qr_ridge = solve(t(R) %*% R + I) %*% (t(x) %*% y_qr)
  beta_qr_ridge = beta_qr_ridge[,1]
  y_qr_hat = x %*% beta_qr_ridge
  y_qr_hat = y_qr_hat[,1]
  
  result = list( call = call, 
                  lambda = lambda, 
                  coef = beta_r,
                  coef_qr = beta_qr_ridge,
                  fitted_values = y_hat,
                  fitted_qr_values = y_qr_hat
  )
  
  class(result) = "ridgereg"
  return(result)
  
}