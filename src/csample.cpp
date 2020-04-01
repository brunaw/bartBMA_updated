#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
#include "utils.h"
#include <RcppArmadilloExtensions/sample.h>
namespace bartBMA { 
IntegerVector utils::csample_num( 
    IntegerVector x,
    int size,
    bool replace, 
    NumericVector prob = NumericVector::create()) {
  RNGScope scope;
  IntegerVector ret = RcppArmadillo::sample(x, size, replace, prob);
  return ret;
}
}

// Write a wrapper to the namespace'd function that will be elevated into R
// [[Rcpp::export]]
IntegerVector csample_num( 
    IntegerVector x,
    int size,
    bool replace, 
    NumericVector prob = NumericVector::create()) {
  
  bartBMA::utils obj;
  
  IntegerVector result = obj.csample_num(x, size, replace, prob);
  return result; 
}