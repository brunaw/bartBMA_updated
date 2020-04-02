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

NumericMatrix utils::add_rows(
    NumericMatrix prior_tree_table_temp, int grow_node){
  
  arma::mat M=Rcpp::as<arma::mat>(prior_tree_table_temp);
  M(grow_node-1,5)=0;
  M(grow_node-1,6)=0;
  M(grow_node-1,0)=grow_node+1;
  M(grow_node-1,1)=grow_node+2;
  M.insert_rows(grow_node,2);
  M(grow_node,4)=-1;
  M(grow_node+1,4)=-1;
  NumericMatrix t=as<NumericMatrix>(wrap(M));
  IntegerVector rname=seq_len(t.nrow());
  
  List dimnms = // two vec. with static names
    List::create(rname,
                 CharacterVector::create("left daughter","right daughter","split var","split point","status","mean","std dev"));
  // and assign it
  t.attr("dimnames") = dimnms;
  return(t);
}
}

// [[Rcpp::export]]
NumericMatrix add_rows(
    NumericMatrix prior_tree_table_temp,
    int grow_node){
  
  bartBMA::utils obj;
  
  NumericMatrix result = obj.add_rows(prior_tree_table_temp, grow_node);
  return result; 
}
//######################################################################################################################//
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


