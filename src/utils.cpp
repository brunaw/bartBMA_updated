#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
#include "utils.h"
#include <RcppArmadilloExtensions/sample.h>
namespace bartBMA { 
// this function doesn't seem to be used anywhere
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

NumericMatrix utils::addcol(
    NumericMatrix prior_tree_matrix_temp, int grow_node,
    NumericVector ld_obs, NumericVector rd_obs){
  
  int ncol = prior_tree_matrix_temp.ncol();
  arma::mat M = Rcpp::as<arma::mat>(prior_tree_matrix_temp);
  
  M.insert_cols(ncol, 1);
  for(int i = 0; i < ld_obs.size(); i++){
    M(ld_obs[i], ncol) = grow_node + 1;
  }
  for(int i = 0; i < rd_obs.size(); i++ ){
    M(rd_obs[i], ncol) = grow_node + 2;
  }
  return(wrap(M));
} 

NumericMatrix utils::set_daughter_to_end_tree(
    int grow_node,
    NumericMatrix prior_tree_table_temp,
    double left_daughter){
  
  int nrow = prior_tree_table_temp.nrow();
  arma::mat M=Rcpp::as<arma::mat>(prior_tree_table_temp);
  M(grow_node-1,5)=0;
  M(grow_node-1,6)=0;
  M.insert_rows(nrow,2);
  
  M(grow_node-1,0)=left_daughter;
  M(grow_node-1,1)=left_daughter+1;
  M(left_daughter-1,4)=-1;
  M(left_daughter,4)=-1;
  // Rcout << "Line 92";
  
  NumericMatrix s=as<NumericMatrix>(wrap(M));
  IntegerVector rname=seq_len(s.nrow());
  
  List dimnms = // two vec. with static names
    List::create(rname,
                 CharacterVector::create("left daughter","right daughter","split var","split point","status","mean","std dev"));
  // and assign it
  s.attr("dimnames") = dimnms;
  
  return(s);
}

NumericMatrix utils::set_daughter_to_end_mat(
    int d, NumericMatrix prior_tree_matrix_temp,
    double left_daughter,
    NumericVector ld_obs,
    NumericVector rd_obs){
  int ncol_mat=prior_tree_matrix_temp.ncol();
  arma::mat N=Rcpp::as<arma::mat>(prior_tree_matrix_temp);
  arma::vec colmat=N.col(d);
  NumericVector colmat2=wrap(colmat);
  
  if(d+1==ncol_mat){
    N.insert_cols(ncol_mat,1);
    int nrow_mat=prior_tree_matrix_temp.nrow();
    NumericVector colmatzero(nrow_mat);
    colmatzero[ld_obs]=left_daughter;
    colmatzero[rd_obs]=left_daughter+1;
    //colmat=Rcpp::as<arma::vec>(colmatzero);
    //N.col(d+1)=colmat;
    N.col(d+1)=Rcpp::as<arma::vec>(colmatzero);
    
  }else{
    //else just update existing column
    colmat2[ld_obs]=left_daughter;
    colmat2[rd_obs]=left_daughter+1;
    //colmat=Rcpp::as<arma::vec>(colmat2);
    //N.col(d)=colmat; 
    N.col(d)=Rcpp::as<arma::vec>(colmat2);
  }
  return(wrap(N));
}

// not sure if this is necessary yet
NumericVector utils::remove_zero(NumericVector nodes_at_depth){
  
  arma::vec nodes_at_depth2 = Rcpp::as<arma::vec>(nodes_at_depth);
  arma::vec ret = nodes_at_depth2.elem(arma::find(nodes_at_depth2!=0));
  
  return(wrap(ret));
}

NumericVector utils::find_term_nodes(NumericMatrix tree_table){
  arma::mat arma_tree(tree_table.begin(),tree_table.nrow(), tree_table.ncol(), false); 
  arma::vec colmat=arma_tree.col(4);
  arma::uvec term_nodes=arma::find(colmat==-1);
  term_nodes=term_nodes+1;
  
  return(wrap(arma::conv_to<arma::vec>::from(term_nodes)));
}
}
// [[Rcpp::export]]
NumericMatrix addcol(
    NumericMatrix prior_tree_matrix_temp, int grow_node,
    NumericVector ld_obs, NumericVector rd_obs){
  
  bartBMA::utils obj;
  
  NumericMatrix result = obj.addcol(
    prior_tree_matrix_temp, 
    grow_node, ld_obs, rd_obs);
  return result; 
}
// [[Rcpp::export]]
NumericMatrix add_rows(
    NumericMatrix prior_tree_table_temp,
    int grow_node){
  
  bartBMA::utils obj;
  
  NumericMatrix result = obj.add_rows(prior_tree_table_temp, grow_node);
  return result; 
}
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
// [[Rcpp::export]]
NumericMatrix set_daughter_to_end_tree(
    int grow_node,
    NumericMatrix prior_tree_table_temp,
    double left_daughter){
  
  bartBMA::utils obj;
  
  NumericMatrix result = obj.set_daughter_to_end_tree(
    grow_node, prior_tree_table_temp, left_daughter);
  return result; 
}  
// [[Rcpp::export]]
NumericMatrix set_daughter_to_end_mat(
    int d, NumericMatrix prior_tree_matrix_temp,
    double left_daughter,
    NumericVector ld_obs,
    NumericVector rd_obs){
  
  bartBMA::utils obj;
  
  NumericMatrix result = obj.set_daughter_to_end_mat(
    d, prior_tree_matrix_temp, left_daughter, ld_obs, rd_obs);
  return result; 
}  
// [[Rcpp::export]]
NumericVector remove_zero(NumericVector nodes_at_depth){
  bartBMA::utils obj;
  
  NumericVector result = obj.remove_zero(nodes_at_depth);
  return result; 
}
// [[Rcpp::export]]
NumericVector find_term_nodes(NumericMatrix tree_table){
  bartBMA::utils obj;
  NumericVector result = obj.find_term_nodes(tree_table);
  return result; 
}
