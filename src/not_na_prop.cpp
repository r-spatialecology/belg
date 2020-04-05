#include <Rcpp.h>
#include <algorithm>     // for count_if
using namespace Rcpp;

// calculates a proportion of cells with NA's
// [[Rcpp::export]]
double not_na_prop(const IntegerMatrix& x) {

  double no_of_cells = x.length();
  double no_of_na = std::count_if(x.begin(), x.end(),
                                  [](double x){return x == NA_INTEGER;});

  return 1 - (no_of_na / no_of_cells);
}

