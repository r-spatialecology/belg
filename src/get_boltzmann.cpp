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

// [[Rcpp::export]]
double get_boltzmann_default(arma::imat x, std::string base, bool relative){
  int min_value = x.min();
  if (min_value < 0 && min_value > INT_MIN){
    // negative values to positive ones
    x = x - min_value;
  } else if (min_value == INT_MIN){
    // NA values in the data = error
    stop("NA values are not supported \n");
  }

  double res = 0;

  while ((x.n_rows != 1) && (x.n_cols != 1)) {
    int num_r = x.n_rows - 1;
    int num_c = x.n_cols - 1;
    arma::imat scaled(num_r, num_c);
    arma::dmat result(num_r, num_c);

    for (int i = 0; i < num_r; i++) {
      for (int j = 0; j < num_c; j++) {
        arma::imat sub_x = x.submat(i, j, i + 1, j + 1);
        arma::ivec sub_x_v = vectorise(sub_x);

        int s = arma::sum(sub_x_v);
        int maxi = arma::max(sub_x_v);
        int mini = arma::min(sub_x_v);

        double temp = (s - maxi - mini) / 2.0;
        int x_a = floor(temp);
        int x_b = ceil(temp);
        int d_a = x_a - mini;
        int d_b = maxi - x_b;
        int d = std::min(d_a, d_b);
        int wu = wu_calc(d, d_a, d_b, x_a, x_b);

        // Conversion + Search for NA values
        arma::vec sub_x_v2 = arma::conv_to<arma::vec>::from(sub_x.elem(find(sub_x != INT_MIN)));
        scaled(i, j) = round(arma::mean(sub_x_v2));

        if (base == "log"){
          result(i, j) = log(wu);
        } else if (base == "log10"){
          result(i, j) = log10(wu);
        } else if (base == "log2"){
          result(i, j) = log2(wu);
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

