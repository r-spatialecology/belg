// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// get_boltzmann_default
double get_boltzmann_default(arma::imat x, std::string base, bool relative);
RcppExport SEXP _belg_get_boltzmann_default(SEXP xSEXP, SEXP baseSEXP, SEXP relativeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::imat >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::string >::type base(baseSEXP);
    Rcpp::traits::input_parameter< bool >::type relative(relativeSEXP);
    rcpp_result_gen = Rcpp::wrap(get_boltzmann_default(x, base, relative));
    return rcpp_result_gen;
END_RCPP
}
// find_na
int find_na(arma::imat sub_x);
RcppExport SEXP _belg_find_na(SEXP sub_xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::imat >::type sub_x(sub_xSEXP);
    rcpp_result_gen = Rcpp::wrap(find_na(sub_x));
    return rcpp_result_gen;
END_RCPP
}
// count_permutations
int count_permutations(arma::vec number);
RcppExport SEXP _belg_count_permutations(SEXP numberSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type number(numberSEXP);
    rcpp_result_gen = Rcpp::wrap(count_permutations(number));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_belg_get_boltzmann_default", (DL_FUNC) &_belg_get_boltzmann_default, 3},
    {"_belg_find_na", (DL_FUNC) &_belg_find_na, 1},
    {"_belg_count_permutations", (DL_FUNC) &_belg_count_permutations, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_belg(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
