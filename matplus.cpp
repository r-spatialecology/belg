#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::imat matplus(arma::imat x){
  int min_value = x.min();
  if (min_value < 0){
    x = x - min_value;
  }
  return(x);
}

// [[Rcpp::export]]
arma::imat naorg(arma::imat x){
  int min_value = x.min();
  if (min_value < 0 && min_value > INT_MIN){
    x = x - min_value;
  } else if (min_value == INT_MIN){
    arma::imat x_minus_one = x - 1;
    int new_min_value = x_minus_one.min();
    if (new_min_value < -1){
      stop("Negative values belbelbe \n");
    }
  }
  return(x);
}

/*** R
# set2e = matrix(c(rep(2, 22), 6, rep(2, 77)), ncol = 10)
# matplus(set2e)
# set2e2 = matrix(c(rep(2, 22), -6, rep(2, 77)), ncol = 10)
# matplus(set2e2)
set2e3 = matrix(c(rep(-1, 22), NA, rep(3, 77)), ncol = 10)

get_boltzmann_default(naorg(set2e3), base = "log", relative = FALSE)
get_boltzmann_default(set2e3, base = "log", relative = FALSE)

# matplus(set2e3)

*/
