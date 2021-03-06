// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// cosine_similarity_mat
arma::sp_mat cosine_similarity_mat(arma::sp_mat X);
RcppExport SEXP _RDataScience_cosine_similarity_mat(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(cosine_similarity_mat(X));
    return rcpp_result_gen;
END_RCPP
}
// cosine_similarity
arma::sp_mat cosine_similarity(arma::sp_mat X);
RcppExport SEXP _RDataScience_cosine_similarity(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(cosine_similarity(X));
    return rcpp_result_gen;
END_RCPP
}
// sma
NumericVector sma(NumericVector x, int n);
RcppExport SEXP _RDataScience_sma(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(sma(x, n));
    return rcpp_result_gen;
END_RCPP
}
// simple_moving_avg
NumericVector simple_moving_avg(DataFrame dt, int n);
RcppExport SEXP _RDataScience_simple_moving_avg(SEXP dtSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type dt(dtSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(simple_moving_avg(dt, n));
    return rcpp_result_gen;
END_RCPP
}
// weighted_moving_avg
NumericVector weighted_moving_avg(DataFrame dt, int n);
RcppExport SEXP _RDataScience_weighted_moving_avg(SEXP dtSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type dt(dtSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(weighted_moving_avg(dt, n));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_RDataScience_cosine_similarity_mat", (DL_FUNC) &_RDataScience_cosine_similarity_mat, 1},
    {"_RDataScience_cosine_similarity", (DL_FUNC) &_RDataScience_cosine_similarity, 1},
    {"_RDataScience_sma", (DL_FUNC) &_RDataScience_sma, 2},
    {"_RDataScience_simple_moving_avg", (DL_FUNC) &_RDataScience_simple_moving_avg, 2},
    {"_RDataScience_weighted_moving_avg", (DL_FUNC) &_RDataScience_weighted_moving_avg, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_RDataScience(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
