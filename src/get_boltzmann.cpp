#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

int wu_calc(int d, int d_a, int d_b, int x_a, int x_b){
  int wu = 0;

  if (d == 0) {
    if (d_a == d_b) {
      if (x_a == x_b) {
        wu = 1;
      } else {
        wu = 6;
      }
    } else {
      if (x_a == x_b) {
        wu = 4;
      } else {
        wu = 12;
      }
    }
  } else {
    if (d_a == d_b) {
      if (x_a == x_b) {
        wu = 12 + 24 * (d - 1) + 6;
      } else {
        wu = 24 + 24 * (d - 1) + 6;
      }
    } else {
      if (x_a == x_b) {
        wu = 12 + 24 * (d - 1) + 12;
      } else {
        wu = 24 + 24 * (d - 1) + 12;
      }
    }
  }
  return(wu);
}

int count_permutations(arma::vec number)
{
  // vector sort
  std::sort(number.begin(), number.end());
  // result init
  int count = 0;
  // iterate for all permutation possible
  do
  {
    count++;
    // generate next permutation until it is possible
  } while(std::next_permutation(number.begin(), number.end()));
  return count;
}

// [[Rcpp::export]]
double get_boltzmann_default(arma::mat x, std::string base, bool relative){
  // float to int
  x = round(x);
  int min_value = x.min();
  if (min_value < 0){
    // negative values to positive ones
    x = x - min_value;
  }
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
          scaled(i, j) = round(arma::mean(sub_x_v));
          if (base == "log"){
            result(i, j) = log(static_cast<double>(wu));
          } else if (base == "log10"){
            result(i, j) = log10(static_cast<double>(wu));
          } else if (base == "log2"){
            result(i, j) = log2(static_cast<double>(wu));
          }
        } else{
          // if all values are NA
          scaled(i, j) = NA_INTEGER;
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
