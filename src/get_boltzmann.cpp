#include <RcppArmadillo.h>
#include "utils.h"

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
double get_boltzmann_default(arma::mat x, std::string base, bool relative){
  // float to int
  x = round(x);
  double res = 0;
  while ((x.n_rows != 1) && (x.n_cols != 1)) {
    int num_r = x.n_rows - 1;
    int num_c = x.n_cols - 1;
    arma::mat scaled(num_r, num_c);
    arma::mat result(num_r, num_c);

    for (int i = 0; i < num_r; i++) {
      for (int j = 0; j < num_c; j++) {
        arma::mat sub_x = x.submat(i, j, i + 1, j + 1);
        // conversion + search for NA values
        arma::vec sub_x_v = arma::conv_to<arma::vec>::from(sub_x.elem(find_finite(sub_x)));
        double wu;
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
          scaled(i, j) = round(arma::mean(sub_x_v));
          if (base == "log"){
            result(i, j) = log(wu);
          } else if (base == "log10"){
            result(i, j) = log10(wu);
          } else if (base == "log2"){
            result(i, j) = log2(wu);
          }
        } else{
          // if all values are NA
          scaled(i, j) = NA_REAL;
          result(i, j) = 0;
        }
      }
    }
    for (int ro = 0; ro < num_r; ro++) {
      for (int co = 0; co < num_c; co++) {
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
ua_pop2 = structure(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA, NA, NA, 27741384, 34436488, 41257648, 41532492, 67418936,
                      68382552, 60985116, 34680164, 8165829, 1549363, 855768, NA, 71125984,
                      71708984, 50149648, 10528977, 10528977, 52469, 42169, NA, NA,
                      NA, NA, NA, 40809992, 30339312, 22183144, 52469, 52469, 52469,
                      47530, NA, NA, NA, NA, NA, 51383, 51997, 52207, 52417, 52469,
                      52469, 47620, NA, NA, NA, NA, NA, 47035, 50108, 51158, 52207,
                      52469, 52469, 47710, NA, NA, NA, NA, NA, 6204001, 48219, 50108,
                      51997, 52469, 52469, 47800, NA, NA, NA, NA, NA, 44794, 49164,
                      50633, 52102, 52469, 52469, 47890, NA, NA, NA, NA, NA, 393510,
                      409565, 414617, 419668, 420930, 420930, 384911, NA, NA, NA, NA,
                      NA, 419307, 420930, 420930, 420930, 420930, 420930, 385632, NA,
                      NA, NA, NA, NA, 420029, 420930, 420930, 420930, 420930, 420930,
                      386354, NA, NA, NA, NA, NA, 420750, 420930, 420930, 420930, 420930,
                      420930, 387076, NA, NA, NA, NA, NA, 420930, 420930, 420930, 420930,
                      420930, 420930, 387797, NA, NA, NA, NA, NA, 56451524, 420930,
                      420930, 420930, 420930, 420930, 388519, NA, NA, NA, NA, NA, 547210304,
                      89190600, 420930, 420930, 420930, 420930, 389240, 145822, NA,
                      NA, NA, NA, 182684048, 420930, 420930, 420930, 420930, 420930,
                      389962, 266088, NA, NA, NA, NA), .Dim = c(x = 12L, y = 27L))
get_boltzmann_default(ua_pop2, base = "log10", relative = TRUE)
*/
