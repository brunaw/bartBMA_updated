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
  
private:  
};
}