.onLoad <- function(libname, pkgname){
    op <- options()
    ## this does nothing as of yet
    op.bygrp <- list(
        ## bygrp.NAopt = FALSE, ## global set of NAopt?
        bygrp.CCheck = FALSE  ## global set of contiguity check?
    )
    toset <- !(names(op.bygrp) %in% names(op))
    if(any(toset)) options(op.bygrp[toset])
    invisible()
}
