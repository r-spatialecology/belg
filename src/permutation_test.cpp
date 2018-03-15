#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int count_permutations(Rcpp::NumericVector number)
{
  // vector sort
  std::sort(number.begin(), number.end());
  // result init
  int count = 0;
  // iterate for all permutation possible
  do
  {
    count++;
    std::cout << number << '\n';
    // generate next permutation until it is possible
  } while(std::next_permutation(number.begin(), number.end()));
  return count;
}

/*** R
count_permutations(c(1, 2, 1))
*/
