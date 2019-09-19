#include <RcppArmadillo.h>
#include "utils.h"

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// https://link.springer.com/article/10.1007%2Fs10980-019-00854-3
// [[Rcpp::export]]
double get_boltzmann_aggregation(arma::mat x, std::string base, bool relative){
  // float to int
  x = round(x);
  double res = 0;

  // check for 2
  if((is_power_of_two(x.n_rows) == 0) | (is_power_of_two(x.n_cols) == 0)){
    throw Rcpp::exception("Number of rows and columns must be a multiple of 2.", false);
  }

  while ((x.n_rows >= 2) && (x.n_cols >= 2)) {
    int new_num_r = x.n_rows / 2;
    int new_num_c = x.n_cols / 2;
    arma::mat scaled(new_num_r, new_num_c);
    arma::mat result(new_num_r, new_num_c);

    int ii = 0;
    for (int i = 0; i < x.n_rows; i = i + 2) {
      int jj = 0;
      for (int j = 0; j < x.n_cols; j = j + 2) {
        arma::mat sub_x = x.submat(i, j, i + 1, j + 1);
        // conversion + search for NA values
        arma::vec sub_x_v = arma::conv_to<arma::vec>::from(sub_x.elem(find_finite(sub_x)));
        int wu;
        if ((sub_x_v.n_elem <= 4) && (sub_x_v.n_elem > 0)){
          if (sub_x_v.n_elem == 4){
            // if there are no NAs
            int s = arma::sum(sub_x_v);
            int maxi = arma::max(sub_x_v);
            int mini = arma::min(sub_x_v);

            double temp = (s - maxi - mini) / 2.0;
            int x_a = floor(temp);
            int x_b = ceil(temp);
            int d_a = x_a - mini;
            int d_b = maxi - x_b;
            int d = std::min(d_a, d_b);
            wu = wu_calc(d, d_a, d_b, x_a, x_b);
          } else if (sub_x_v.n_elem > 1){
            // if there are between one and two NAs
            wu = count_permutations(sub_x_v);
          } else if (sub_x_v.n_elem == 1){
            // if three values are NA
            wu = 1;
          }
          // Rcout << "The value of jj : " << jj << "\n";
          // Rcout << "The value of a ii : " << ii << "\n";
          scaled(ii, jj) = round(arma::mean(sub_x_v));
          if (base == "log"){
            result(ii, jj) = log(static_cast<double>(wu));
          } else if (base == "log10"){
            result(ii, jj) = log10(static_cast<double>(wu));
          } else if (base == "log2"){
            result(ii, jj) = log2(static_cast<double>(wu));
          }
        } else{
          // if all values are NA
          scaled(ii, jj) = NA_REAL;
          result(ii, jj) = 0;
        }
        jj++;
      }
      ii++;
    }
    for (int ro = 0; ro < new_num_r; ro++) {
      for (int co = 0; co < new_num_c; co++) {
        res += result(ro, co);
      }
    }
    if (relative == true){
      break;
    } else {
      x = scaled;
    }
  }
  return(res);
}

/*** R
new_c = c(56, 86, 98, 50, 45, 56, 96, 25,
          15, 55, 85, 69, 12, 52, 25, 56,
          32, 25, 68, 98, 58, 66, 56, 58,
          15, 55, 85, 69, 12, 52, 25, 56)
lg = matrix(new_c, nrow = 4, ncol = 8, byrow = TRUE)

lg = matrix(c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4)), ncol = 4, nrow = 4)

new_c = c(56, 86, 98, 50, 45, 56, 96, 25,
          15, 55, 85, 69, 12, 52, 25, 56,
          32, 25, 68, 98, 58, 66, 56, 58)
lg = matrix(new_c, nrow = 3, ncol = 8, byrow = TRUE)

get_boltzmann_aggregation(lg, base = "log10", relative = FALSE)
*/
