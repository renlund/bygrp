// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// contiguous
bool contiguous(IntegerVector g, bool error);
RcppExport SEXP _bygrp_contiguous(SEXP gSEXP, SEXP errorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type g(gSEXP);
    Rcpp::traits::input_parameter< bool >::type error(errorSEXP);
    rcpp_result_gen = Rcpp::wrap(contiguous(g, error));
    return rcpp_result_gen;
END_RCPP
}
// sum_g
NumericVector sum_g(NumericVector x, IntegerVector g, bool na_rm, bool na_opt, bool no_na);
RcppExport SEXP _bygrp_sum_g(SEXP xSEXP, SEXP gSEXP, SEXP na_rmSEXP, SEXP na_optSEXP, SEXP no_naSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type g(gSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    Rcpp::traits::input_parameter< bool >::type na_opt(na_optSEXP);
    Rcpp::traits::input_parameter< bool >::type no_na(no_naSEXP);
    rcpp_result_gen = Rcpp::wrap(sum_g(x, g, na_rm, na_opt, no_na));
    return rcpp_result_gen;
END_RCPP
}
// sumi_g
IntegerVector sumi_g(IntegerVector x, IntegerVector g, bool na_rm, bool na_opt, bool no_na);
RcppExport SEXP _bygrp_sumi_g(SEXP xSEXP, SEXP gSEXP, SEXP na_rmSEXP, SEXP na_optSEXP, SEXP no_naSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type g(gSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    Rcpp::traits::input_parameter< bool >::type na_opt(na_optSEXP);
    Rcpp::traits::input_parameter< bool >::type no_na(no_naSEXP);
    rcpp_result_gen = Rcpp::wrap(sumi_g(x, g, na_rm, na_opt, no_na));
    return rcpp_result_gen;
END_RCPP
}
// max_g
NumericVector max_g(NumericVector x, IntegerVector g, bool na_rm, bool na_opt, bool no_na);
RcppExport SEXP _bygrp_max_g(SEXP xSEXP, SEXP gSEXP, SEXP na_rmSEXP, SEXP na_optSEXP, SEXP no_naSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type g(gSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    Rcpp::traits::input_parameter< bool >::type na_opt(na_optSEXP);
    Rcpp::traits::input_parameter< bool >::type no_na(no_naSEXP);
    rcpp_result_gen = Rcpp::wrap(max_g(x, g, na_rm, na_opt, no_na));
    return rcpp_result_gen;
END_RCPP
}
// maxi_g
IntegerVector maxi_g(IntegerVector x, IntegerVector g, bool na_rm, bool na_opt, bool no_na);
RcppExport SEXP _bygrp_maxi_g(SEXP xSEXP, SEXP gSEXP, SEXP na_rmSEXP, SEXP na_optSEXP, SEXP no_naSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type g(gSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    Rcpp::traits::input_parameter< bool >::type na_opt(na_optSEXP);
    Rcpp::traits::input_parameter< bool >::type no_na(no_naSEXP);
    rcpp_result_gen = Rcpp::wrap(maxi_g(x, g, na_rm, na_opt, no_na));
    return rcpp_result_gen;
END_RCPP
}
// min_g
NumericVector min_g(NumericVector x, IntegerVector g, bool na_rm, bool na_opt, bool no_na);
RcppExport SEXP _bygrp_min_g(SEXP xSEXP, SEXP gSEXP, SEXP na_rmSEXP, SEXP na_optSEXP, SEXP no_naSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type g(gSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    Rcpp::traits::input_parameter< bool >::type na_opt(na_optSEXP);
    Rcpp::traits::input_parameter< bool >::type no_na(no_naSEXP);
    rcpp_result_gen = Rcpp::wrap(min_g(x, g, na_rm, na_opt, no_na));
    return rcpp_result_gen;
END_RCPP
}
// mini_g
IntegerVector mini_g(IntegerVector x, IntegerVector g, bool na_rm, bool na_opt, bool no_na);
RcppExport SEXP _bygrp_mini_g(SEXP xSEXP, SEXP gSEXP, SEXP na_rmSEXP, SEXP na_optSEXP, SEXP no_naSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type g(gSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    Rcpp::traits::input_parameter< bool >::type na_opt(na_optSEXP);
    Rcpp::traits::input_parameter< bool >::type no_na(no_naSEXP);
    rcpp_result_gen = Rcpp::wrap(mini_g(x, g, na_rm, na_opt, no_na));
    return rcpp_result_gen;
END_RCPP
}
// dup_g
LogicalVector dup_g(SEXP v, IntegerVector g, bool na_rm);
RcppExport SEXP _bygrp_dup_g(SEXP vSEXP, SEXP gSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type v(vSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type g(gSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(dup_g(v, g, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// g_id
IntegerVector g_id(CharacterVector gch);
RcppExport SEXP _bygrp_g_id(SEXP gchSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type gch(gchSEXP);
    rcpp_result_gen = Rcpp::wrap(g_id(gch));
    return rcpp_result_gen;
END_RCPP
}
// g_n
IntegerVector g_n(IntegerVector g);
RcppExport SEXP _bygrp_g_n(SEXP gSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type g(gSEXP);
    rcpp_result_gen = Rcpp::wrap(g_n(g));
    return rcpp_result_gen;
END_RCPP
}
// g_row
IntegerVector g_row(IntegerVector g);
RcppExport SEXP _bygrp_g_row(SEXP gSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type g(gSEXP);
    rcpp_result_gen = Rcpp::wrap(g_row(g));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_bygrp_contiguous", (DL_FUNC) &_bygrp_contiguous, 2},
    {"_bygrp_sum_g", (DL_FUNC) &_bygrp_sum_g, 5},
    {"_bygrp_sumi_g", (DL_FUNC) &_bygrp_sumi_g, 5},
    {"_bygrp_max_g", (DL_FUNC) &_bygrp_max_g, 5},
    {"_bygrp_maxi_g", (DL_FUNC) &_bygrp_maxi_g, 5},
    {"_bygrp_min_g", (DL_FUNC) &_bygrp_min_g, 5},
    {"_bygrp_mini_g", (DL_FUNC) &_bygrp_mini_g, 5},
    {"_bygrp_dup_g", (DL_FUNC) &_bygrp_dup_g, 3},
    {"_bygrp_g_id", (DL_FUNC) &_bygrp_g_id, 1},
    {"_bygrp_g_n", (DL_FUNC) &_bygrp_g_n, 1},
    {"_bygrp_g_row", (DL_FUNC) &_bygrp_g_row, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_bygrp(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
