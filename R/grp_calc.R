##' Shift vector
##'
##' Create shifted vector.
##' @param x A vector.
##' @param fill Value to use for padding when the window goes beyond the input
##'     length.
##' @name shift
NULL

##' @rdname shift
##' @details lag1: calculate lag by 1.
##' @export
lag1 <- function(x, fill = NA){
    c(fill, x[-length(x)])
}

##' @rdname shift
##' @details lead1: calculate lead by 1.
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
##' @details grp_check: check the grouping variable
##' @export
grp_check <- function(grp){
    cl = class(grp)
    cls <- paste0("{", paste0(cl, collapse = ", "), "}")
    if(length(cl) > 1){
        s <- paste0("grp has more than one class, maybe not a problem")
        warning(s)
    }
    if(!is.integer(grp)){
        s <- paste0("grp has class '", cls, "', use integer ",
                    "grouping variable for optimal speed")
        message(s)
    }
    if(is.numeric(grp) & !is.integer(grp)) {
        grp_org <- grp
        grp <- as.integer(grp)
        if(!all(grp_org == grp)){
            s <- paste0("numeric grouping will be treated as integer BUT ",
                        "contains non-integer values")
            stop(s)
        }
    }
    grp <- grp_id(grp)
    if(!contigous(grp, error = FALSE)){
        s <- paste0("grouping is not sufficiently ordered, it needs to be ",
                    "contiguous (have its unique values next to each other)")
        stop(s)
    } else {
        message("grouping is sufficiently ordered (contiguous)")
    }
    invisible(NULL)
}

##' @rdname grp_calc
##' @details grp_id: calculate an integer valued grouping vector
##' @export
grp_id <- function(grp){
    cl <- cl0 <- class(grp)
    if(length(cl) > 1) {
        cl <- if(is.integer(grp)){
                  "integer"
              } else if(is.numeric(grp)){
                  "numeric"
              } else if(is.factor(grp)){
                  "factor"
              } else if(is.character(grp)){
                  "character"
              } else "other"
    }
    switch(
        EXPR = cl,
        integer = grp,
        factor = as.integer(grp),
        numeric = {
            warning("numeric grouping will be 'as.integer'")
            as.integer(grp)
        },
        character = g_id(gch = grp),
        stop("no supported class in {", paste0(cl0, collapse = ", "), "}")
    )
}

##' @rdname grp_calc
##' @details grp_n: calculate the number of rows per grouping value.
##' @export
grp_n <- function(grp){
    if(!is.integer(grp)) grp <- grp_id(grp)
    g_n(g = grp)
}

##' @rdname grp_calc
##' @details grp_last: calculate an indicator for last row per grouping value.
##' @export
grp_last <- function(grp){
    c((grp != lead1(grp))[-length(grp)], TRUE)
}

##' @rdname grp_calc
##' @details grp_first: calculate an indicator for first row per grouping value.
##' @export
grp_first <- function(grp){
    c(TRUE, (grp !=lag1(grp))[-1])
}

##' @rdname grp_calc
##' @details grp_row: calculate the row number per grouping value.
##' @export
grp_row <- function(grp){
    if(!is.integer(grp)) grp <- grp_id(grp)
    g_row(g = grp)
}
