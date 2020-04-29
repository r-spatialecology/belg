#include <RcppArmadillo.h>
#include "utils.h"

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
double wu_calc(int d, int d_a, int d_b, int x_a, int x_b){
  double wu = 0.0;

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
        wu = 12 + 24.L * (d - 1) + 6;
      } else {
        wu = 24 + 24.L * (d - 1) + 6;
      }
    } else {
      if (x_a == x_b) {
        wu = 12 + 24.L * (d - 1) + 12;
      } else {
        wu = 24 + 24.L * (d - 1) + 12;
      }
    }
  }
  return(wu);
}

int count_permutations(arma::vec number){
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

int is_power_of_two(unsigned int x){
  while (((x % 2) == 0) && x > 1)
    x /= 2;
  return (x == 1);
}

/*** R
d = 135516394
d_a = 135516394
d_b = 411272980
x_a = 135937324
x_b = 135937324
wu_calc(d, d_a, d_b, x_a, x_b)

is_power_of_two(2)
is_power_of_two(4)
is_power_of_two(8)
is_power_of_two(16)
is_power_of_two(17)
*/
