#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
int find_na(arma::imat sub_x){
  arma::ivec sub_x_v = vectorise(sub_x);
  arma::vec sub_x_v2 = arma::conv_to<arma::vec>::from(sub_x.elem(find(sub_x != INT_MIN)));
  int size = sub_x_v2.n_elem;
  return(size);
}

/*** R
mat1 = matrix(c(1, 0, NA, 1), ncol = 2)
mat2 = matrix(c(1, 0, 0, 1), ncol = 2)

find_na(mat1)
find_na(mat2)
*/
