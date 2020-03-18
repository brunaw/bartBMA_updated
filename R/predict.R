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
  
  #---------------------------------------------------------------------

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

  if(x$type == "regression"){
    class(predictions)<- "predict.bartBMA"
    return(predictions)
    
  }
  else if(x$type == "classification"){
    probs <- pnorm(predictions)
    
    binary_prediction <- ifelse(probs <= 0.5, 0, 1)
    
    return_prediction <- list(
      probs = probs,
      binary_prediction = binary_prediction)
  
    class(binary_prediction)<- "predict.bartBMA"
    return(binary_prediction)
    
  }

}