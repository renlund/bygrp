##' C++ functions (mostly for internal use)
##'
##' These are the functions implemented in C++ (via Rcpp).
##' @param g integer valued (grouping) vector
##' @param gch character valued (grouping) vector
##' @param x numeric (integer) vector
##' @param v vector of values
##' @param na_rm boolean; remove missing values?
##' @param na_opt boolean; if na_rm is TRUE, should a grouping
##'   value containing only missing x-values get summation NA (TRUE)
##'   or else the standard value 0 (sum), -Inf (max), and Inf (min).
##' @param no_na boolean; can be set to TRUE if missing values are impossible
##' @name CppFnc
NULL
