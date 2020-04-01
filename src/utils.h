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
private:  
};
}