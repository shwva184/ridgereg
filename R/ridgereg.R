#' Perform ridge regression using ordinary least squares as well as QR decomposition
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

#' This contains the print methods for ridgereg function.
#' 
#' 
#' @param x An object of ridgereg class
#' @param ... Further arguments passed to or from other methods
#' @export


print.ridgereg = function(x,...){
  if (!inherits(x, "ridgereg"))
    stop("This is not a \"ridgereg\" object.")
  if(length(x$coef)){
    cat("Call:\n ")
    print.default(as.vector(x$call))
    cat("\n Coefficent is \n")
    print.default(t(x$coef))
  } else {cat("Coefficient not available \n")}
}

#' This contains the fitted values of ridgereg function.
#' 
#' @param p An object of ridgereg class
#' @param ... Further arguments passed to or from other methods
#' @export

pred.ridgereg = function(p,...){
  if (!inherits(p, "ridgereg")){
    stop("This is not a \"ridgereg\" object.")}
  return(as.vector(p$fitted_values))
}

#' This contains the regression coefficents of ridgereg function.
#' 
#' 
#' @param object An object of ridgereg class
#' @param ... Further arguments passed to or from other methods
#' @export

coef.ridgereg = function(object, ...){
  if (!inherits(object, "ridgereg"))
    stop("This is not a \"ridgereg\" object.")
  if(length(object$coef)){
    print.default(t(object$coef))
  } else {cat("Coefficient not available \n")}
}
