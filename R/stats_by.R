##' Fast statistics within grouping
##'
##' \code{X_by} are functions to calculate the statistic X under a grouping
##' variable specified by grp. Important: x and grp are assumed to be ordered on
##' grp such that equal values appear contiguously, and there should be no
##' missing values in grp.
##' @param x A vector.
##' @param grp A grouping vector within values of which the statistic is to be
##'     calculated. Should not contain missing values.
##' @param na.rm A logical value; remove missing values
##' @param NAopt A logical value; if missing values are removed
##'     (i.e. \code{na.rm = TRUE}), a TRUE here means that the result is NA if
##'     all values of x within a fixed grouping value is missing, else R default
##'     behaviour is used with 0 (sum), -Inf (max) or Inf (min).
##' @param noNA A logical value; in some cases x cannot contain missing values,
##'     then this parameter can be set to TRUE to (possibly) gain some speed.
##' @param check A logical value; check that grp is contiguous?
##' @seealso \code{\link{grp_calc}}
##' @name stats_by
NULL

##' @rdname stats_by
##' @details sum_by: summation.
##' @export
sum_by <- function(x, grp, na.rm = FALSE, NAopt = FALSE, noNA = FALSE, check = FALSE){
    if(!is.integer(grp)) grp <- grp_id(grp)
    if(check) contiguous(g = grp, error = TRUE)
    ## ------------------------------
    if(is.integer(x)){
        sumi_g(x = x, g = grp, na_rm = na.rm, na_opt = NAopt, no_na = noNA)
    } else {
        sum_g(x = x, g = grp, na_rm = na.rm, na_opt = NAopt, no_na = noNA)
    }
}

##' @rdname stats_by
##' @details max_by: maximum.
##' @export
max_by <- function(x, grp, na.rm = FALSE, NAopt = FALSE, noNA = FALSE, check = FALSE){
    if(!is.integer(grp)) grp <- grp_id(grp)
    if(check) contiguous(g = grp, error = TRUE)
    possibly_inf <- na.rm & !NAopt ## integer function cannot handle infinity
    ## ------------------------------
    if(is.integer(x) & !possibly_inf){
        maxi_g(x = x, g = grp, na_rm = na.rm, na_opt = NAopt, no_na = noNA)
    } else {
        max_g(x = x, g = grp, na_rm = na.rm, na_opt = NAopt, no_na = noNA)
    }
}

##' @rdname stats_by
##' @details min_by: minimum.
##' @export
min_by <- function(x, grp, na.rm = FALSE, NAopt = FALSE, noNA = FALSE, check = FALSE){
    if(!is.integer(grp)) grp <- grp_id(grp)
    if(check) contiguous(g = grp, error = TRUE)
    possibly_inf <- na.rm & !NAopt
    ## ------------------------------
    if(is.integer(x) & !possibly_inf){
        mini_g(x = x, g = grp, na_rm = na.rm, na_opt = NAopt, no_na = noNA)
    } else {
        min_g(x = x, g = grp, na_rm = na.rm, na_opt = NAopt, no_na = noNA)
    }
}

##' @rdname stats_by
##' @details duplicated_by: looks for duplicated values. If \code{na.rm = TRUE},
##'     then missing values are never considered duplicates.
##' @export
duplicated_by <- function(x, grp, na.rm = FALSE, check = FALSE){
    if(!is.integer(grp)) grp <- grp_id(grp)
    if(check) contiguous(g = grp, error = TRUE)
    ## ------------------------------
    dup_g(v = x, g = grp, na_rm = na.rm)
}

##' @rdname stats_by
##' @details n_by: calculates number of rows. If \code{na.rm = TRUE} it will not
##'     count rows where x is missing.
##' @export
n_by <- function(x, grp, na.rm = FALSE, check = FALSE){
    if(!is.integer(grp)) grp <- grp_id(grp)
    if(check) contiguous(g = grp, error = TRUE)
    ## ------------------------------
    if(na.rm){
        sumi_g(x = as.integer(!is.na(x)), g = grp, na_rm = FALSE,
              na_opt = FALSE, no_na = TRUE)
    } else {
        g_n(g = grp)
    }
}

##' @rdname stats_by
##' @details uniqueN_by: number of unique values.
##' @export
uniqueN_by <- function(x, grp, na.rm = FALSE, check = FALSE){
    if(!is.integer(grp)) grp <- grp_id(grp)
    if(check) contiguous(g = grp, error = TRUE)
    ## ------------------------------
    n <- sumi_g(x = if(na.rm) as.integer(!is.na(x)) else rep(1L, length(x)),
                g = grp, na_rm = FALSE, na_opt = FALSE, no_na = TRUE)
    m <- sumi_g(x = as.integer(dup_g(v = x, g = grp, na_rm = na.rm)),
                g = grp, na_rm = FALSE, na_opt = FALSE, no_na = TRUE)
    n-m
}

##' @rdname stats_by
##' @details anyTRUE_by: checks for any TRUE value. Note that NAs
##'     will count as FALSE.
##' @export
anyTRUE_by <- function(x, grp, check = FALSE){
    sum_by(x = as.numeric(x & !is.na(x)), grp = grp, noNA = TRUE, check = check) > 0
}

##' @rdname stats_by
##' @details lag1_by: lag by 1.
##' @export
lag1_by <- function(x, grp, check = FALSE){
    if(check){
        if(!is.integer(grp)) grp <- grp_id(grp)
        contiguous(g = grp, error = TRUE)
    }
    ## ------------------------------
    i <- grp != lag1(x = grp)
    i[1] <- TRUE
    l <- lag1(x = x)
    l[i] <- NA
    l
}

##' @rdname stats_by
##' @details lead1_by: lead by 1.
##' @export
lead1_by <- function(x, grp, check = FALSE){
    if(check){
        if(!is.integer(grp)) grp <- grp_id(grp)
        contiguous(g = grp, error = TRUE)
    }
    ## ------------------------------
    i <- grp != lead1(x = grp)
    i[length(i)] <- TRUE
    l <- lead1(x = x)
    l[i] <- NA
    l
}
