#include "RcppArmadillo.h"
using namespace Rcpp ;
namespace bartBMA {
class utils
{
public:
  Rcpp::IntegerVector csample_num(
      Rcpp::IntegerVector x,
      int size,
      bool replace, 
      Rcpp::NumericVector prob
  );
  Rcpp::NumericMatrix add_rows(
      Rcpp::NumericMatrix prior_tree_table_temp,
      int grow_node
  );
  Rcpp::NumericMatrix addcol(
      Rcpp::NumericMatrix prior_tree_matrix_temp, 
      int grow_node,
      Rcpp::NumericVector ld_obs, 
      Rcpp::NumericVector rd_obs
    );
  Rcpp::NumericMatrix set_daughter_to_end_tree(
      int grow_node,
      Rcpp::NumericMatrix prior_tree_table_temp,
      double left_daughter
    );
  Rcpp::NumericMatrix set_daughter_to_end_mat(
      int d, 
      Rcpp::NumericMatrix prior_tree_matrix_temp,
      double left_daughter,
      Rcpp::NumericVector ld_obs,
      Rcpp::NumericVector rd_obs
    );
  Rcpp::NumericVector remove_zero(NumericVector nodes_at_depth);
    
private:  
};
}