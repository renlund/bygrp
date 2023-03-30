# bygrp

This package contains functions for fast *but stupid* calculations of selected
statistics under a grouping.

Before we explain the stupidity, lets see what is does. Suppose you want to sum
the variable `x` for each value of `g` in the data frame `df` defined here:

```r
df <- data.frame(x =  10:15, g = rep(1:2, each = 3))
```

This can be achieved with `bygrp::sum_by`:

```r
library(bygrp)
df$sum.x.by.g <- sum_by(x = df$x, grp = df$g)
print(df)
#>    x g sum.x.by.g
#> 1 10 1         33
#> 2 11 1         33
#> 3 12 1         33
#> 4 13 2         42
#> 5 14 2         42
#> 6 15 2         42
```

and this will be fast with `sum_by` which is implemented in C++. *Fast* is the
only reason to use this function (for more on this, see the vignette), as the
functionality already exists in more versatile functions, e.g. `stats::ave`:

```r
df2 <- df
df2$sum.x.by.g <- ave(x = df$x, df$g, FUN = sum)
```

or in `data.table`:

```r
library(data.table)
dt <- as.data.table(df)[, sum.x.by.g := sum(x), by = g]
```

or `dplyr`:

```r
library(dplyr)
dp <- tibble(df) %>%
    group_by(g) %>%
    mutate(sum.x.by.g = sum(x)) %>%
    ungroup()
```

## The stupidity
There is more than one layer of stupidity!

### Grouping must already be contiguous

The grouping must already be arranged such that equal values of `grp` must be
next to each other. When in doubt, you can check `grp` with the function
`contiguous` or set the argument `check` to `TRUE`. E.g. the following will
yield an error:

```r
sum_by(1:3, grp = c(2,1,2), check = TRUE)
#> Error in contiguous(g = grp, error = TRUE) : grouping not contiguous
```

### More general expression will **not** be evaluated under the grouping

Suppose you want to sum the smallest and largest value for each group. This can
be done with `ave`

```r
ave(df2$x, df2$g, FUN = function(z) min(z) + max(z))
#> [1] 22 22 22 28 28 28
```

or data.table

```r
dt[, min.p.max := min(x) + max(x), by = g]
```

But the following will **not** yield the desired result, it will in fact throw
an error

```r
sum_by(x = min(df$x) + max(df$x), grp = df$g)
#> Error in sumi_g(x = x, g = grp, na_rm = na.rm, na_opt = NAopt, no_na = noNA) :
#>  grouping not of same length as input
```

The argument `x` must simply be the vector of values (and not an expression) for
which the summation (under the grouping provided by `grp`) is to be calculated.

As a side note, this particular problem *can* be solved with other functions in
the package:

```r
min_by(df$x, grp = df$g) + max_by(df$x, grp = df$g)
```
