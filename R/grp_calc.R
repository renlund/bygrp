##' Shift vector
##'
##' Create shifted vector.
##' @param x A vector.
##' @param fill Value to use for padding when the window goes beyond the input
##'     length.
##' @name shift
NULL

##' @rdname shift
##' @details lag1 calcualtes lag by 1.
##' @export
lag1 <- function(x, fill = NA){
    c(fill, x[-length(x)])
}

##' @rdname shift
##' @details lead1 calculates lead by 1.
##' @export
lead1 <- function(x, fill = NA){
    c(x[-1], fill)
}

##' Calculations on grouping vector
##'
##' Various functions to calculate things for a grouping vector. Important: grp
##' is assumed to be ordered such that equal values appear contiguously.
##' @param grp grouping vector
##' @examples
##' id = sprintf("id%s", c(2,2,1,4,4,4))
##' data.frame(
##'     id = id,
##'     grp_id = grp_id(id),
##'     grp_n = grp_n(id),
##'     grp_row = grp_row(id),
##'     grp_first = grp_first(id),
##'     grp_last = grp_last(id)
##' )
##' @seealso \code{\link{stats_by}}
##' @name grp_calc
NULL

##' @rdname grp_calc
##' @details grp_id calculates an integer valued grouping vector starting at 1.
##' @export
grp_id <- function(grp){
    rl <- rle(grp)
    rep.int(x = 1:length(rl$values), times = rl$lengths)
}

##' @rdname grp_calc
##' @details grp_n calculates the number of rows per grouping value.
##' @export
grp_n <- function(grp){
    rl <- rle(grp)
    rep.int(x = rl$lengths, times = rl$lengths)
}

##' @rdname grp_calc
##' @details grp_last calculates an indicator for last row per grouping value.
##' @export
grp_last <- function(grp){
    ## c(head(grp != lead1(grp), -1), TRUE)
    c((grp != lead1(grp))[-length(grp)], TRUE)
}

##' @rdname grp_calc
##' @details grp_first calculates an indicator for first row per grouping value.
##' @export
grp_first <- function(grp){
    c(TRUE, (grp !=lag1(grp))[-1])
}

##' @rdname grp_calc
##' @details grp_row calculates the row number per grouping value.
##' @export
grp_row <- function(grp){
    a <- seq_len(length.out = length(grp))
    1 + a - rep.int(x = a[grp_first(grp)], times = rle(grp)$lengths)
}
