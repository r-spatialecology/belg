#ifndef utils_H
#define utils_H
#include "Rcpp.h"

double wu_calc(int d, int d_a, int d_b, int x_a, int x_b);
int count_permutations(arma::vec number);
int is_power_of_two(unsigned int x);

#endif // utils_H
