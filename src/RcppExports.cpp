// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// lastdose_impl
Rcpp::List lastdose_impl(Rcpp::NumericVector id, Rcpp::NumericVector time, Rcpp::NumericVector amt, Rcpp::NumericVector evid, Rcpp::NumericVector addl, Rcpp::NumericVector ii, Rcpp::NumericVector fill, Rcpp::LogicalVector back_calc, Rcpp::LogicalVector sort1, Rcpp::LogicalVector comment, bool fill_comment_na);
RcppExport SEXP _lastdose_lastdose_impl(SEXP idSEXP, SEXP timeSEXP, SEXP amtSEXP, SEXP evidSEXP, SEXP addlSEXP, SEXP iiSEXP, SEXP fillSEXP, SEXP back_calcSEXP, SEXP sort1SEXP, SEXP commentSEXP, SEXP fill_comment_naSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type id(idSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type time(timeSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type amt(amtSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type evid(evidSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type addl(addlSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type ii(iiSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type fill(fillSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type back_calc(back_calcSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type sort1(sort1SEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type comment(commentSEXP);
    Rcpp::traits::input_parameter< bool >::type fill_comment_na(fill_comment_naSEXP);
    rcpp_result_gen = Rcpp::wrap(lastdose_impl(id, time, amt, evid, addl, ii, fill, back_calc, sort1, comment, fill_comment_na));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_lastdose_lastdose_impl", (DL_FUNC) &_lastdose_lastdose_impl, 11},
    {NULL, NULL, 0}
};

RcppExport void R_init_lastdose(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
