#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

int wu_calc(int d, int d_a, int d_b, int x_a, int x_b){
  int Wu = 0;

  if (d == 0) {
    if (d_a == d_b) {
      if (x_a == x_b) {
        Wu = 1;
      } else {
        Wu = 6;
      }
    } else {
      if (x_a == x_b) {
        Wu = 4;
      } else {
        Wu = 12;
      }
    }
  } else {
    if (d_a == d_b) {
      if (x_a == x_b) {
        Wu = 24 * (d - 1) + 18;
      } else {
        Wu = 24 * (d - 1) + 30;
      }
    } else {
      if (x_a == x_b) {
        Wu = 24 * (d - 1) + 24;
      } else {
        Wu = 24 * (d - 1) + 36;
      }
    }
  }
  return(Wu);
}

//' Boltzmann entropy of a landscape gradient;
//'
//' @param x A matrix.
//' @param base A logarithm base ("log", "log2" or "log10")
//' @param relative TRUE/FALSE
//' @references Gao, Peichao, Hong Zhang, and Zhilin Li. "A hierarchy-based
//' solution to calculate the configurational entropy of landscape gradients."
//' Landscape Ecology 32.6 (2017): 1133-1146.
//' @references Gao, Peichao, Hong Zhang, and Zhilin Li. "An efficient
//' analytical method for computing the Boltzmann entropy of a landscape
//' gradient." Transactions in GIS (2018).
//' @export
// [[Rcpp::export]]
double get_boltzmann(arma::mat x, std::string base = "log", bool relative = false){
  double Res = 0;

  while ((x.n_rows > 1) && (x.n_cols > 1)) {
    int num_r = x.n_rows - 1;
    int num_c = x.n_cols - 1;
    arma::mat Scaled(num_r, num_c);
    arma::mat Result(num_r, num_c);

    for (int i = 0; i < num_r; i++) {
      for (int j = 0; j < num_c; j++) {
        arma::mat Scaledtmp = x.submat(i, j, i + 1, j + 1);
        arma::vec v = vectorise(Scaledtmp);

        Scaled(i, j) = mean(v);
        int s = arma::sum(v);
        int maxi = arma::max(v);
        int mini = arma::min(v);

        double temp = (s - maxi - mini) / 2.0;
        int x_a = std::floor(temp);
        int x_b = std::ceil(temp);
        int d_a = x_a - mini;
        int d_b = maxi - x_b;
        int d = std::min(d_a, d_b);
        int Wu = wu_calc(d, d_a, d_b, x_a, x_b);
        if (base == "log"){
          Result(i, j) = log(Wu);
        } else if (base == "log10"){
          Result(i, j) = log10(Wu);
        } else if (base == "log2"){
          Result(i, j) = log2(Wu);
        }
      }
    }
    for (int ro = 0; ro < num_r; ro++) {
      for (int co = 0; co < num_c; co++) {
        Res += Result(ro, co);
      }
    }
    if (relative == true){
      break;
    } else {
      x = Scaled;
    }
  }
  return(Res);
}
