#' Perform ridge regression using ordinary least squares as well as QR decomposition
#' 
#' @param formula an object, that is symbolic description of the model to be fitted.
#' @param data a data frame.
#' @param lambda the lambda of the data set(hyperparameter). Default is 0.
#' @importFrom stats model.matrix sd coef
#' @return An object of class ridgereg
#' 
#'
#' @export
#' 

ridgereg = function(formula,data,lambda = 0){
  stopifnot("This is not a formula" = class(formula)=="formula")
  stopifnot("lambda is not numeric" = is.numeric(lambda))
  call=match.call()
  x = model.matrix(formula,data)
  value = all.vars(formula)[1]
  y = data[,value]
  bo = mean(y)
  scales = apply(x, 2, sd)[2:ncol(x)]
  m = apply(x, 2, mean)[2:ncol(x)]
  #normalizing x
  
  x = scale(x[,-1])
  I = diag(ncol(x))
  
  #computation using least squares 
  
  #beta_r = solve(t(x) %*% x + lambda * I) %*% t(x) %*% y
  #y_hat = x %*% beta_r + bo
  
  #computation using QR
  l = ncol(x)
  y_qr = rbind(as.matrix(data[all.vars(formula)[1]]),matrix(0,nrow = l,ncol = 1))
  x_qr = rbind(x,diag(sqrt(lambda),nrow=l,ncol=l))
  qr = qr(x_qr)
  R = qr.R(qr)
  Q = qr.Q(qr)
  beta_r = solve(R) %*% t(Q) %*% y_qr
  y_hat = x %*% beta_r + bo
  
  result = list( call = call, 
                 lambda = lambda, 
                 coefficients = beta_r,
                 #coef_qr = beta_rr,
                 fitted_values = y_hat,
                 bo = bo,
                 scales =scales,
                 m = m
                 #fitted_qr_values = y_qr_hat
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
  cat("Call:\n ")
  print.default(as.vector(x$call))
  cat("\n Coefficent is \n")
  print(coef(x))
}

#' This contains the fitted values of ridgereg function.
#' 
#' @param object An object of ridgereg class
#' @param newdata this is newdata
#' @param ... Further arguments passed to or from other methods
#' @export

<<<<<<< HEAD
predict.ridgereg <- function(object, newdata = NULL, ...){
  if(is.null(newdata)){
    fitted_values <- drop(object$fitted_values)
  } else {
    if(all(unlist(lapply(newdata, is.numeric)))){
      i <- which(colnames(newdata) %in% rownames(object$coefficients))
      x <- scale(newdata[, i])
      fitted_values <- x %*% object$coefficients + object$bo
      
    } else {
      stop("All columns must be numeric!")
    }
  }
  return(fitted_values)
=======
predict.ridgereg = function(object,...){
  if (!inherits(object, "ridgereg")){
    stop("This is not a \"ridgereg\" object.")}
  
  return(as.vector(object$fitted_values))
>>>>>>> 0b380ee59d8b9e07c66be79669c9c7f89a98419c
}

#' This contains the regression coefficents of ridgereg function.
#' 
#' 
#' @param object An object of ridgereg class
#' @param ... Further arguments passed to or from other methods
#' @export

coef.ridgereg = function(object, ...)
{
  scaledcoef = t(as.matrix(object$coefficients / object$scales))
  inter = object$bo - scaledcoef %*% object$m
  scaledcoef = cbind(Intercept=inter, scaledcoef)
  return(drop(scaledcoef))
}
