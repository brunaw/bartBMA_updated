#-----------------------------------------------------------------------
# Predict functions for the whole package 
#-----------------------------------------------------------------------


# Standard predict

#' @title Predictions for a new dataset using an existing bartbma object
#' 
#' @description This function produces predictions for a new dataset using a previously obtained bartBMA object.
#' @param x A bartBMA object obtained using the barBMA function.
#' @param newdata Covariate matrix for new dataset.
#' @export 
#' @return A vector of predictions for the new dataset.
#' @examples 
#' set.seed(100)
#' #simulate some data
#' N <- 100
#' p<- 100
#' epsilon <- rnorm(N)
#' xcov <- matrix(runif(N*p), nrow=N)
#' y <- sin(pi*xcov[,1]*xcov[,2]) + 20*(xcov[,3]-0.5)^2+10*xcov[,4]+5*xcov[,5]+epsilon
#' epsilontest <- rnorm(N)
#' xcovtest <- matrix(runif(N*p), nrow=N)
#' ytest <- sin(pi*xcovtest[,1]*xcovtest[,2]) + 20*(xcovtest[,3]-0.5)^2+10*xcovtest[,4]+
#'   5*xcovtest[,5]+epsilontest
#' 
#' #Train the object 
#' bart_bma_example <- bartBMA(x.train = xcov,y.train=y,x.test=xcovtest,zero_split = 1, 
#'                             only_max_num_trees = 1,split_rule_node = 0)
#' #Obtain the prediction intervals
#' predict(bart_bma_example, newdata = xcovtest)

predict.bartBMA <- function(x, newdata, ...){
  #preds<-get_BART_BMA_test_predictions(newdata,object$bic,object$sumoftrees,object$y_minmax)
  #orig_preds<-preds[[1]]
  #class(orig_preds)<-"predict.bartBMA"
  #orig_preds
  
  if(!class(newdata) == "matrix"){
    
    newdata <- as.matrix(newdata)
    
  }
  
  if(is.null(newdata) && length(x) == 16){
    #if test data specified separately
    predictions <- preds_bbma_lin_alg_outsamp(
      x$sumoftrees,
      x$obs_to_termNodesMatrix,
      x$response,
      x$bic, 0,  0,
      x$nrowTrain,
      nrow(x$newdata),
      x$a,
      x$sigma,
      0,
      x$nu,
      x$lambda,#diff_inital_resids,
      x$newdata
    )
  } else { 
    if(is.null(newdata) && length(x)==14){
      #else return Pred Ints for training data
      predictions <-  preds_bbma_lin_alg_insamp(
        x$sumoftrees,
        x$obs_to_termNodesMatrix,
        x$response,
        x$bic,
        0, 0,
        x$nrowTrain,
        x$a,
        x$sigma,
        0,
        x$nu,
        x$lambda
      )
    }
    else{
      #if test data included in call to x
      predictions <-  preds_bbma_lin_alg_outsamp(
        x$sumoftrees, # List
        x$obs_to_termNodesMatrix, # List
        x$response,
        x$bic, 0, 0,
        x$nrowTrain,
        nrow(newdata), 
        x$a,
        x$sigma,
        0,
        x$nu,
        x$lambda,
        newdata
      )
    }}
  
  
  class(predictions)<- "predict.bartBMA"
  
  return(predictions)
  
}


#-----------------------------------------------------------------------
#  predict_probit_bartBMA

#' @title Predictions for a new dataset using an existing probit_bartBMA object
#' 
#' @description This function produces predictions for a new dataset using a previously obtained bartBMA object.
#' @param object A probit_bartBMA object obtained using the probit_bartBMA function.
#' @param newdata Covariate matrix for new dataset.
#' @export 
#' @return The output is a list of length 2:
#' \item{probs}{A vector of estimated probabilities for newdata.} 
#' \item{pred_binary}{A vector of binary predictions for newdata.} 
#' @examples 
#' #Example from BART package (McCulloch et al. 2019)
#' set.seed(99)
#' n=100
#' x = sort(-2+4*runif(n))
#' X=matrix(x,ncol=1)
#' f = function(x) {return((1/2)*x^3)}
#' FL = function(x) {return(exp(x)/(1+exp(x)))}
#' pv = FL(f(x))
#' y = rbinom(n,1,pv)
#' 
#' trained_probit_bbma <- probit_bartBMA(x.train = X,y.train = y)
#' 
#' np=100
#' xp=-2+4*(1:np)/np
#' Xp=matrix(xp,ncol=1)
#' 
#' predict_probit_bartBMA(trained_probit_bbma,Xp)

predict_probit_bartBMA<-function(object,newdata){
  
  
  #preds<-get_BART_BMA_test_predictions(newdata,object$bic,object$sumoftrees,object$y_minmax)
  
  
  
  if(is.null(newdata) && length(object)==16){
    #if test data specified separately
    preds<-preds_bbma_lin_alg_outsamp(object$sumoftrees,object$obs_to_termNodesMatrix,object$response,object$bic,0, 0,object$nrowTrain,
                                      nrow(object$test_data),object$a,object$sigma,0,object$nu,
                                      object$lambda,#diff_inital_resids,
                                      object$test_data
    )
  }else{if(is.null(newdata) && length(object)==14){
    #else return Pred Ints for training data
    preds<-preds_bbma_lin_alg_insamp(object$sumoftrees,object$obs_to_termNodesMatrix,object$response,object$bic,0, 0,object$nrowTrain,
                                     object$a,object$sigma,0,object$nu,
                                     object$lambda#diff_inital_resids
    )
    
  }else{
    #if test data included in call to object
    preds<-preds_bbma_lin_alg_outsamp(object$sumoftrees,object$obs_to_termNodesMatrix,object$response,object$bic,0, 0,object$nrowTrain,
                                      nrow(newdata), object$a,object$sigma,0,object$nu,
                                      object$lambda,#diff_inital_resids,
                                      newdata
    )
  }}
  
  
  
  
  
  
  
  
  ret <- list()
  
  #probs <-pnorm(preds[[1]])
  probs <-pnorm(preds)
  
  pred_binary <- ifelse(probs<=0.5,0,1)
  
  ret$probs <- probs
  ret$pred_binary <- pred_binary
  
  ret
}
