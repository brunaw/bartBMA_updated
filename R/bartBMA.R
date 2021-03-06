
#' @title Bayesian Additive Regression Trees Using Bayesian Model Averaging (BART-BMA)
#' 
#' @description This is an implementation of Bayesian Additive Regression Trees \insertCite{chipman2010bart}{bartBMA} using Bayesian Model Averaging \insertCite{hernandez2018bayesian}{bartBMA}. 
#' @param formula Object of class \code{formula describing the model equation
#' @param data Training data of class, in a rectangular format (\code{data.frame}, \code{matrix}, etc). 
#' @param a This is a parameter that influences the variance of terminal node parameter values. Default value a=3.
#' @param nu This is a hyperparameter in the distribution of the variance of the error term. THe inverse of the variance is distributed as Gamma (nu/2, nu*lambda/2). Default value nu=3.
#' @param sigquant Calibration quantile for the inverse chi-squared prior on the variance of the error term.
#' @param c This determines the size of Occam's Window
#' @param pen This is a parameter used by the Pruned Exact Linear Time Algorithm when finding changepoints. Default value pen=12.
#' @param num_cp This is a number between 0 and 100 that determines the proportion of changepoints proposed by the changepoint detection algorithm to keep when growing trees. Default num_cp=20.
#' @param x.test Test data covariate matrix. Default x.test=matrix(0.0,0,0).
#' @param num_rounds Number of trees. (Maximum number of trees in a sum-of-tree model). Default num_rounds=5.
#' @param alpha Parameter in prior probability of tree node splitting. Default alpha=0.95
#' @param beta Parameter in prior probability of tree node splitting. Default beta=1
#' @param split_rule_node logical. If `TRUE`, then find a new set of potential splitting points via a changepoint algorithm after adding each split to a tree. If `FALSE`, use the same set of potential split points for all splits in a tree. Default split_rule_node= `TRUE`.
#' @param cp_method Sets the rule finding method as grid search
#' ("gridpoint") or the Pruned Exact Linear time ("PELT"). 
#' @param maxOWsize Maximum number of models to keep in Occam's window. Default maxOWsize=100.
#' @param num_splits Maximum number of splits in a tree
#' @param gridsize This integer determines the size of the grid across which to search if cp_method = "gridpoint" when finding changepoints for constructing trees.
#' @param zero_split logical. If `TRUE`, then zero split trees can be included in a sum-of-trees model. If `FALSE`, then only trees with at least one split can be included in a sum-of-trees model.
#' @param only_max_num_trees Binary variable. If equals 1, then only sum-of-trees models containing the maximum number of trees, num_rounds, are selected. If equals 0, then sum-of-trees models containing less than num_rounds trees can be selected. The default is only_max_num_trees=1.
#' @param min_num_obs_for_split This integer determines the minimum number of observations in a (parent) tree node for the algorithm to consider potential splits of the node.
#' @param min_num_obs_after_split This integer determines the minimum number of observations in a child node resulting from a split in order for a split to occur. If the left or right chikd node has less than this number of observations, then the split can not occur.
#' @param exact_residuals Binary variable. If equal to 1, then trees are added to sum-of-tree models within each round of the algorithm by detecting changepoints in the exact residuals. If equals zero, then changepoints are detected in residuals that are constructed from approximate predictions.
#' @param spike_tree If equal to 1, then the Spike-and-Tree prior will be used, otherwise the standard BART prior will be used. The number of splitting variables has a beta-binomial prior. The number of terminal nodes has a truncated Poisson prior, and then a uniform prior is placed on the set of valid constructions of trees given the splitting variables and number of terminal nodes.
#' @param s_t_hyperprior If equals 1 and spike_tree equals 1, then a beta distribution hyperprior is placed on the variable inclusion probabilities for the spike and tree prior. The hyperprior parameters are a_s_t and b_s_t.
#' @param p_s_t If spike_tree=1 and s_t_hyperprior=0, then p_s_t is the prior variable inclusion probability.
#' @param a_s_t If spike_tree=1 and s_t_hyperprior=1, then a_s_t is a parameter of a beta distribution hyperprior.
#' @param b_s_t If spike_tree=1 and s_t_hyperprior=1, then b_s_t is a parameter of a beta distribution hyperprior.
#' @param lambda_poisson This is a parameter for the Spike-and-Tree prior. It is the parameter for the (truncated and conditional on the number of splitting variables) Poisson prior on the number of terminal nodes.
#' @param less_greedy If equal to one, then a less greedy model search algorithm is used.
#' @param ... Further arguments.
#' @rdname bartBMA
#' @references
#' \insertAllCited{}
#' @export 
#' @return The following objects are returned by bartbma:
#' \item{fitted.values}{The vector of predictions of the outcome for all training observations.} 
#' \item{sumoftrees}{This is a list of lists of matrices. The outer list corresponds to a list of sum-of-tree models, and each element of the outer list is a list of matrices describing the structure of the trees within a sum-of-tree model. See details.} 
#' \item{obs_to_termNodesMatrix}{This is a list of lists of matrices. The outer list corresponds to a list of sum-of-tree models, and each element of the outer list is a list of matrices describing to which node each of the observations is allocated to at all depths of each tree within a sum-of-tree model. See details.} 
#' \item{bic}{This is a vector of BICs for each sum-of-tree model.} 
#' \item{test.preds}{A vector of test data predictions. This output only is given if there is test data in the input.} 
#' \item{sum_residuals}{CURRENTLY INCORRECT OUTPUT. A List (over sum-of-tree models) of lists (over single trees in a model) of vectors of partial residuals. Unless the maximum number of trees in a model is one, in which case the output is a list (over single tree models) of vectors of partial residuals, which are all equal to the outcome vector.} 
#' \item{numvars}{This is the total number of variables in the input training data matrix.} 
#' \item{call}{match.call returns a call in which all of the specified arguments are specified by their full names.} 
#' \item{y_minmax}{Range of the input training data outcome vector.}
#' \item{response}{Input taining data outcome vector.}
#' \item{nrowTrain}{number of observations in the input training data.}
#' \item{sigma}{sd(y)/(max(y)-min(y))}
#' \item{a}{input parameter}
#' \item{nu}{input parameter}
#' \item{lambda}{parameter determined by the inputs sigma, sigquant, and nu}
#' @useDynLib bartBMA, .registration = TRUE
#' @importFrom Rdpack reprompt 
#' @importFrom Rcpp evalCpp
#' @importFrom stats sd qchisq quantile qnorm qchisq pnorm
#' @examples
#' N <- 100
#' p<- 100
#' set.seed(100)
#' library(bartBMA)
#' epsilon <- rnorm(N)
#' xcov <- matrix(runif(N*p), nrow=N)
#' y <- sin(pi*xcov[,1]*xcov[,2]) + 20*(xcov[,3]-0.5)^2+10*xcov[,4]+
#' 5*xcov[,5]+epsilon
#' epsilontest <- rnorm(N)
#' xcovtest <- matrix(runif(N*p), nrow=N)
#' ytest <- sin(pi*xcovtest[,1]*xcovtest[,2]) + 20*(xcovtest[,3]-0.5)^2+10*xcovtest[,4]+
#' 5*xcovtest[,5]+epsilontest
#' bart_bma_example <- bartBMA(x = xcov,y=y,x.test=xcovtest,zero_split = 1, 
#' only_max_num_trees = 1,split_rule_node = 0)


bartBMA <- function(x,...) UseMethod("bartBMA", x)

#' @rdname bartBMA
#' @export bartBMA.default
#' @export
#'
bartBMA.default <- function(formula, 
                            data,
                            a = 3, nu = 3,
                            sigquant = 0.9,
                            c = 1000,
                            pen = 12,
                            num_cp = 20,
                            x.test = matrix(0.0,0,0),
                            num_rounds = 5,
                            alpha = 0.95, beta = 2, 
                            split_rule_node = FALSE,
                            cp_method = "PELT",
                            maxOWsize = 100,
                            num_splits = 5,
                            gridsize = 10,
                            zero_split = TRUE,
                            only_max_num_trees = 1,
                            min_num_obs_for_split = 2, 
                            min_num_obs_after_split = 2,
                            exact_residuals = 1,
                            spike_tree = 0, 
                            s_t_hyperprior = 1, 
                            p_s_t = 0.5, a_s_t = 1,b_s_t = 3,
                            lambda_poisson = 10,less_greedy = 0,
                            x = NULL, y = NULL, 
                            ...){
  
  #---------------------------------------------------------------------
  ## Formula interface: extracting response and covariates 
  if (is.null(formula)) {
    if (is.null(y) | is.null(x)) {
      stop("Error: Please provide a formula or an x matrix and a y vector.")
    } 
  } else {
    formula <- formula(formula)
    if (!inherits(formula, "formula")) {
      stop("Error: Invalid formula.")
    }
    f <- as.formula(formula)
    t <- terms(f, data = data)
    
    ## Get dependent var
    response <- data.frame(eval(f[[2]], envir = data, enclos = parent.frame()))
    colnames(response) <- deparse(f[[2]])
    
    ## Get independent vars (disconsidering interactions for now)
    independent_vars <- attr(t, "term.labels")
    interaction_idx <- grepl(":", independent_vars)
    
    main_vars <- independent_vars[!interaction_idx]
    dat_main <- data[, main_vars, drop = FALSE]
    
    data.selected <- data.frame(response, 
                                dat_main, check.names = FALSE)
    
    y <- data.selected[, 1]
    # This is not dealing with factor covariates yet (!!!)
    x <- as.matrix(data.selected[, -1, drop = FALSE])
  }
  
  #---------------------------------------------------------------------
  # Parameter definitions
  
  
  split_rule_node <- 1 * split_rule_node
  zero_split <- 1 * zero_split
  
  # Change point method 
  gridpoint <- ifelse(cp_method == "PELT", 0, 1)
  
  num_obs = nrow(x)
  num_vars = ncol(x)
  
  # Priors'hyperparameters
  binary = FALSE
  start_mean = 0
  start_sd = 1
  mu = 0
  sigma_mu = 0
  sigma = sd(y)/(max(y)-min(y))
  qchi = qchisq(1.0 - sigquant, nu, 1, 0)
  lambda = (sigma * sigma * qchi) / nu
  
  
  #---------------------------------------------------------------------
  # Standard checkings on the input data 
  
  # 1. Factors are transformed into numeric?
  if(is.factor(y)) {
    # if(length(levels(y)) != 2) stop("y is a factor with number of levels != 2")
    binary = TRUE
    #  y = as.numeric(y)-1
    stop("Response must be a numeric vector")
  } else {
    if((length(unique(y)) == 2) & (max(y) == 1) & (min(y) == 0)) {
      warning('NOTE: assumming numeric response is binary\n')
      binary = TRUE
      #stop("Response must be a numeric vector")
    }
  }
  if(gridsize < 1) stop("gridsize must be a positive integer")
  
  # 3. Always have a matrix of predictors 
  if(is.vector(x) | is.factor(x)| is.data.frame(x)) x = as.matrix(x)
  
  #if(is.vector(x.test) | is.factor(x.test)| is.data.frame(x.test)) x.test = as.matrix(x.test)
  
  # check input arguments:
  # if((!is.matrix(x)) || (typeof(x)!="double")) stop("argument x must be a double matrix")
  # if((!is.matrix(x.test)) || (typeof(x.test)!="double")) stop("argument x.test must be a double matrix")
  if((!is.matrix(x))) stop("argument x must be a double matrix")
  if((!is.matrix(x.test)) ) stop("argument x.test must be a double matrix")
  if(!binary) {
    if((!is.vector(y))) stop("argument y must be a double vector")
  }
  
  # 2. Checking that the number of observations for the 
  # predictors is the same as for the response variables
  
  
  if(nrow(x) != length(y)) stop("number of rows in x must equal length of y")
  if((nrow(x.test) >0) && (ncol(x.test)!=ncol(x))) stop("input x.test must have the same number of columns as x")
  #if((!is.na(sigest)) && (typeof(sigest)!="double")) stop("input sigest must be double")
  #if((!is.na(sigest)) && (sigest<0.0)) stop("input sigest must be positive")
  #if((mode(sigdf)!="numeric") || (sigdf<0)) stop("input sigdf must be a positive number")
  
  # 4. Parameter checkings 
  if(c < 1) {
    stop("Value of Occam's Window has to be greater than 0.")
  }
  
  if(num_cp < 0 || num_cp > 100){
    stop("Value of num_cp should be a value between 1 and 100.")
  }
  
  #---------------------------------------------------------------------
  # Calling main function
  
  y_classes <- length(unique(y))
  
  if(y_classes == 2){
    type <- "classification"
  } else {
    type <- "regression"
  }
  
  if(type == "regression"){
  # Regression
  
  bartBMA_call = BART_BMA_sumLikelihood(
    less_greedy,
    spike_tree, 
    s_t_hyperprior, 
    p_s_t, a_s_t, b_s_t,
    num_obs, num_vars, lambda_poisson,
    x, y, start_mean, start_sd,
    a, mu, nu, lambda, c, sigma_mu,
    pen, num_cp, x.test, num_rounds, alpha, beta, split_rule_node,
    gridpoint, maxOWsize, num_splits, gridsize, zero_split, only_max_num_trees,
    min_num_obs_for_split, min_num_obs_after_split, exact_residuals)
  
  if(length(bartBMA_call) == 6){
    #length of bartBMA_call is 6 if test data was included in the call
    names(bartBMA_call) <- c("fitted.values","sumoftrees","obs_to_termNodesMatrix","bic","test.preds","sum_residuals")
    bartBMA_call$test_data<-x.test
  } else {
    names(bartBMA_call) <- c("fitted.values","sumoftrees",
                             "obs_to_termNodesMatrix","bic","sum_residuals")
  }

  additional_regression <- list(
    numvars = ncol(x), 
    call = match.call(), 
    y_minmax = range(y),
    response = y, 
    nrowTrain = nrow(x),
    sigma = sigma, 
    a = a, nu = nu, lambda = lambda
  )  

  bartBMA_call <- c(bartBMA_call, additional_regression)
  
  } 
  else if(type == "classification"){
  
  #---------------------------------------------------------------------
  # Classification
  
  Zlatent <- ifelse(y == 1, 3.1, -3.1) # ?
  binary = FALSE
  start_mean = 0
  start_sd = 1
  mu = 0
  sigma_mu = 0
  sigma = sd(Zlatent)/(max(Zlatent) - min(Zlatent))
  qchi = qchisq(1.0 - sigquant, nu, 1, 0)
  lambda = (sigma* sigma* qchi)/nu
  
  if(is.factor(Zlatent)) {
    binary = TRUE
    #  Zlatent = as.numeric(Zlatent)-1
    stop("Response must be a numeric vector")
  } else {
    if((length(unique(Zlatent)) == 2) & 
       (max(Zlatent) == 1) & (min(Zlatent) == 0)) {
      cat('NOTE: assumming numeric response is binary\n')
      binary = TRUE
    }
  }

  if(is.vector(x) | is.factor(x)| is.data.frame(x)) x = as.matrix(x)
  if(is.vector(x.test) | is.factor(x.test)| is.data.frame(x.test)) x.test = as.matrix(x.test)
  
  if(is.matrix(x)) {
    if(nrow(x.test)>0) {
      if(!is.matrix(x.test)) stop('x.test must be a matrix')
    } 
  }
  # check input arguments:
  # if((!is.matrix(x)) || (typeof(x)!="double")) stop("argument x must be a double matrix")
  # if((!is.matrix(x.test)) || (typeof(x.test)!="double")) stop("argument x.test must be a double matrix")
  if((!is.matrix(x))) stop("argument x must be a double matrix")
  if((!is.matrix(x.test)) ) stop("argument x.test must be a double matrix")
  if(!binary) {
    if((!is.vector(Zlatent))) stop("argument Zlatent must be a double vector")
  }
  if(nrow(x) != length(Zlatent)) stop("number of rows in x must equal length of Zlatent")
  if((nrow(x.test) >0) && (ncol(x.test)!=ncol(x))) stop("input x.test must have the same number of columns as x")
  #if((!is.na(sigest)) && (typeof(sigest)!="double")) stop("input sigest must be double")
  #if((!is.na(sigest)) && (sigest<0.0)) stop("input sigest must be positive")
  #if((mode(sigdf)!="numeric") || (sigdf<0)) stop("input sigdf must be a positive number")
  if(c<1)stop("Value of Occam's Window has to be greater than 0."); 
  if(num_cp<0 || num_cp>100)stop("Value of num_cp should be a value between 1 and 100."); 
  
  bartBMA_call = BART_BMA_sumLikelihood(
    less_greedy,
    spike_tree, s_t_hyperprior, 
    p_s_t, a_s_t, b_s_t,
    num_obs, num_vars, lambda_poisson,
    x, Zlatent, start_mean,
    start_sd, a, mu, nu, lambda, c, sigma_mu,
    pen, num_cp, x.test, num_rounds, 
    alpha, beta, split_rule_node,
    gridpoint, maxOWsize,
    num_splits, gridsize, zero_split,
    only_max_num_trees, min_num_obs_for_split, 
    min_num_obs_after_split, exact_residuals)
  
  if(length(bartBMA_call)==6){
    #length of bartBMA_call is 6 if test data was included in the call
    names(bartBMA_call)<-c("fitted.values","sumoftrees","obs_to_termNodesMatrix","bic","test.preds","sum_residuals")
    bartBMA_call$test_data <- x.test
  }else{
    names(bartBMA_call) <- c("fitted.values","sumoftrees",
                             "obs_to_termNodesMatrix","bic","sum_residuals")
  }
  
  additional_classification <- list(
    numvars = ncol(x), 
    call = match.call(), 
    y_minmax = range(y),
    response = y, 
    nrowTrain = nrow(x),
    sigma = sigma, 
    a = a, nu = nu, lambda = lambda, 
    fitted.probs = pnorm(bartBMA_call$fitted.values), 
    fitted.classes = ifelse(bartBMA_call$fitted.probs <=0.5, 0, 1)
  )  
  
  bartBMA_call <- c(bartBMA_call, additional_classification)
  
  }
  
  #---------------------------------------------------------------------
  bartBMA_call$type  <- type
  # Defining class
  class(bartBMA_call) <- "bartBMA"
  
  return(bartBMA_call)
  #---------------------------------------------------------------------
  
}

#' A print function for the bartBMA method. 
#' 
#' 
#' @title print.bartBMA
#' @param x A bartBMA model
#' @export 

print.bartBMA <- function(x, ...){
  
  if(x$type == "regression"){
  x$prediction.error <- sqrt(sum((x$fitted.values  - x$response)^2))
  x$prediction.error <- round(x$prediction.error, 3)
  }
  else if(x$type == "classification"){
    binary_prediction <- ifelse(x$fitted.probs <= 0.5, 0, 1)
    x$prediction.error <- round(1 - sum(binary_prediction == x$response)/length(x$response), 3) 
    x$prediction.error <- paste0(x$prediction.error * 100, "%")
  }
  
  cat("bartBMA ", x$type, " result \n \n")
  cat("Call:\n")
  print(x$call)
  cat("\n")
  cat(paste0("Type:\t\t\t\t  ", "Regression\n"))
  cat(paste0("Sample Size:\t\t\t  ", x$nrowTrain, "\n"))
  cat(paste0("Number of features:\t\t  ", x$numvars, "\n"))
  cat(paste0("Prior parameters (sigma, a, nu):  ", "(", round(x$sigma, 2), ",", x$a,  ",", x$nu, ")", "\n"))
  cat(paste0("Number of Sum of Trees:\t\t  ", length(x$sumoftrees), "\n"))
  cat(paste0("Prediction error:\t\t  ", x$prediction.error, "\n"))
  cat(paste0("Pseudo R-Squared:\t\t  ", 0, "\n"))
  invisible(x)
}

