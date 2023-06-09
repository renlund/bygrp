% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppDoc.R, R/RcppExports.R
\name{CppFnc}
\alias{CppFnc}
\alias{contiguous}
\alias{sum_g}
\alias{sumi_g}
\alias{max_g}
\alias{maxi_g}
\alias{min_g}
\alias{mini_g}
\alias{dup_g}
\alias{g_id}
\alias{g_n}
\alias{g_row}
\title{C++ functions (mostly for internal use)}
\usage{
contiguous(g, error = FALSE)

sum_g(x, g, na_rm = FALSE, na_opt = FALSE, no_na = FALSE)

sumi_g(x, g, na_rm = FALSE, na_opt = FALSE, no_na = FALSE)

max_g(x, g, na_rm = FALSE, na_opt = FALSE, no_na = FALSE)

maxi_g(x, g, na_rm = FALSE, na_opt = FALSE, no_na = FALSE)

min_g(x, g, na_rm = FALSE, na_opt = FALSE, no_na = FALSE)

mini_g(x, g, na_rm = FALSE, na_opt = FALSE, no_na = FALSE)

dup_g(v, g, na_rm = FALSE)

g_id(gch)

g_n(g)

g_row(g)
}
\arguments{
\item{g}{integer valued (grouping) vector}

\item{x}{numeric (integer) vector}

\item{na_rm}{boolean; remove missing values?}

\item{na_opt}{boolean; if na_rm is TRUE, should a grouping value containing
only missing x-values get NA as a result (\code{no_opt = TRUE}), else
the result is the standard values 0 (sum), -Inf (max), or Inf (min).}

\item{no_na}{boolean; can be set to TRUE if missing values are impossible}

\item{v}{vector of values}

\item{gch}{character valued (grouping) vector}
}
\description{
These are the functions implemented in C++ (via Rcpp).
}
\details{
contiguous: this function checks if g's unique values are
contiguously arranged

sum_g: sum numeric values by group

sumi_g: sum integer values by group

max_g: calculate the maximum numeric value by group

maxi_g: calculate the maximum integer value by group

min_g: calculate the minimum numeric value by group

mini_g: calculate the minimum integer value by group

dup_g: calculate a logical vector indicating duplicated values in x by group

g_id: character grouping as integers

g_n: number of observations by group

g_row: row number by group
}
