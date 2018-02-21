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
        Wu = 12 + 24 * (d - 1) + 6;
      } else {
        Wu = 24 + 24 * (d - 1) + 6;
      }
    } else {
      if (x_a == x_b) {
        Wu = 12 + 24 * (d - 1) + 12;
      } else {
        Wu = 24 + 24 * (d - 1) + 12;
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
double get_boltzmann2(arma::imat x, std::string base = "log10", bool relative = true){
  double Res = 0;

  while ((x.n_rows != 1) && (x.n_cols != 1)) {
    int num_r = x.n_rows - 1;
    int num_c = x.n_cols - 1;
    arma::imat Scaled(num_r, num_c);
    arma::dmat Result(num_r, num_c);

    for (int i = 0; i < num_r; i++) {
      for (int j = 0; j < num_c; j++) {
        arma::imat Subx = x.submat(i, j, i + 1, j + 1);
        arma::ivec v = vectorise(Subx);

        int s = arma::sum(v);
        int maxi = arma::max(v);
        int mini = arma::min(v);

        double temp = (s - maxi - mini) / 2.0;
        int x_a = floor(temp);
        int x_b = ceil(temp);
        int d_a = x_a - mini;
        int d_b = maxi - x_b;
        int d = std::min(d_a, d_b);
        int Wu = wu_calc(d, d_a, d_b, x_a, x_b);
        double xxx = mean(v);
        Scaled(i, j) = std::round(xxx);

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
    Rcout << "Res: " << Res << "\n";
    if (relative == true){
      break;
    } else {
      x.print("Pre:");
      x = Scaled;
      x.print("Post:");
    }
  }
  return(Res);
}
