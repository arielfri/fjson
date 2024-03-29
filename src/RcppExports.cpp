// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// fjson_parse_string
List fjson_parse_string(std::string string);
RcppExport SEXP _fjson_fjson_parse_string(SEXP stringSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type string(stringSEXP);
    rcpp_result_gen = Rcpp::wrap(fjson_parse_string(string));
    return rcpp_result_gen;
END_RCPP
}
// fjson_parse_vector
List fjson_parse_vector(std::vector<std::string> strings);
RcppExport SEXP _fjson_fjson_parse_vector(SEXP stringsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type strings(stringsSEXP);
    rcpp_result_gen = Rcpp::wrap(fjson_parse_vector(strings));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_fjson_fjson_parse_string", (DL_FUNC) &_fjson_fjson_parse_string, 1},
    {"_fjson_fjson_parse_vector", (DL_FUNC) &_fjson_fjson_parse_vector, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_fjson(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
