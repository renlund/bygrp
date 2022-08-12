##' Fast statistics within grouping
##'
##' \code{X_by} are functions to calculate the statistic X under a grouping
##' variable specified by grp. Important: x and grp are assumed to be ordered on
##' grp such that equal values appear consecutively, and there should be no
##' missing values in grp.
##' @param x A vector.
##' @param grp A grouping vector within values of which the statistic is to be
##'     calculated. Should not contain missing values.
##' @param na.rm A logical value; remove missing values
##' @param NAopt A logical value; if missing values are removed, should the
##'     result still be NA for the cases where x is completely missing?
##' @param noNA A logical value; in some cases x cannot contain missing values,
##'     then this parameter can be set to TRUE to gain some speed.
##' @seealso \code{\link{grp_calc}}
##' @name stats_by
NULL

##' @rdname stats_by
##' @details sum_by does summation.
##' @export
sum_by <- function(x, grp, na.rm = FALSE, NAopt = FALSE, noNA = FALSE){
    if(!is.integer(grp)) grp <- data.table::rleid(grp)
    contiguous(g = grp, error = TRUE)
    ## ------------------------------
    sum_g(x = x, g = grp, na_rm = na.rm, na_opt = NAopt, no_na = noNA)
}

##' @rdname stats_by
##' @details max_by calculates the maximum.
##' @export
max_by <- function(x, grp, na.rm = FALSE, NAopt = FALSE, noNA = FALSE){
    if(!is.integer(grp)) grp <- data.table::rleid(grp)
    contiguous(g = grp, error = TRUE)
    ## ------------------------------
    max_g(x = x, g = grp, na_rm = na.rm, na_opt = NAopt, no_na = noNA)
}

##' @rdname stats_by
##' @details min_by calculates the minimum.
##' @export
min_by <- function(x, grp, na.rm = FALSE, NAopt = FALSE, noNA = FALSE){
    if(!is.integer(grp)) grp <- data.table::rleid(grp)
    contiguous(g = grp, error = TRUE)
    ## ------------------------------
    min_g(x = x, g = grp, na_rm = na.rm, na_opt = NAopt, no_na = noNA)
}

##' @rdname stats_by
##' @details duplicated_by looks for duplicated values. If
##'     \code{na.rm = TRUE}, then missing values are never considered
##'     duplicates.
##' @export
duplicated_by <- function(x, grp, na.rm = FALSE){
    if(!is.integer(grp)) grp <- data.table::rleid(grp)
    contiguous(g = grp, error = TRUE)
    ## ------------------------------
    dup_g(x = x, g = grp, na_rm = na.rm)
}

##' @rdname stats_by
##' @details n_by calculates number of rows. If \code{na.rm = TRUE} it will not
##'     count rows where x is missing.
##' @export
n_by <- function(x, grp, na.rm = FALSE){
    if(!is.integer(grp)) grp <- data.table::rleid(grp)
    contiguous(g = grp, error = TRUE)
    ## ------------------------------
    sum_g(x = if(na.rm) as.integer(!is.na(x)) else rep(1L, length(x)),
          g = grp, na_rm = FALSE, na_opt = FALSE, no_na = TRUE)
}

##' @rdname stats_by
##' @details duplicated_by looks for duplicated values. If
##'     \code{na.rm = TRUE}, then missing values are never considered
##'     duplicates.
##' @export
uniqueN_by <- function(x, grp, na.rm = FALSE){
    if(!is.integer(grp)) grp <- data.table::rleid(grp)
    contiguous(g = grp, error = TRUE)
    ## ------------------------------
    n <- sum_g(x = if(na.rm) as.integer(!is.na(x)) else rep(1L, length(x)),
               g = grp, na_rm = FALSE, na_opt = FALSE, no_na = TRUE)
    m <- sum_g(x = as.integer(dup_g(x = x, g = grp, na_rm = na.rm)),
               g = grp, na_rm = FALSE, na_opt = FALSE, no_na = TRUE)
    n-m
}

##' @rdname stats_by
##' @details anyTRUE_by checks for any TRUE value. Note that NAs
##'     will count as FALSE.
##' @export
anyTRUE_by <- function(x, grp){
    sum_by(x = as.numeric(x & !is.na(x)), grp = grp, noNA = TRUE) > 0
}

##' @rdname stats_by
##' @details lag1_by is lag by 1.
##' @export
lag1_by <- function(x, grp){
    i <- grp != lag1(x = grp)
    i[1] <- TRUE
    l <- lag1(x = x)
    l[i] <- NA
    l
}

##' @rdname stats_by
##' @details lead1_by is lead by 1.
##' @export
lead1_by <- function(x, grp){
    i <- grp != lead1(x = grp)
    i[length(i)] <- TRUE
    l <- lead1(x = x)
    l[i] <- NA
    l
}
