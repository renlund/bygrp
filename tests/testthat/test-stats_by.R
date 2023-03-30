test_that("sum_by works", {

    ## something standard
    g <- as.integer(c(7,7,7, 12, 1,1, 6,6,6,6))
    x <- c(1:3, -1, 11:12, 100:103)
    expect_equal(sum_by(x, grp = g),
                 c(6,6,6, -1, 23,23, 406,406,406,406))

    ## standard, but x is integer?
    x <- as.integer(x)
    expect_equal(sum_by(x, grp = g),
                 c(6,6,6, -1, 23,23, 406,406,406,406))
    expect_equal(sum_by(x, grp = g, noNA = TRUE),
                 c(6,6,6, -1, 23,23, 406,406,406,406))

    ## sum_by works with missing
    g <- as.integer(c(7,7,7, 12, 1,1, 6,6,6,6))
    x <- c(1:3, NA_real_, 11:12, NA_real_,101:103)
    expect_equal(sum_by(x, grp = g, na.rm = FALSE, NAopt = FALSE),
                 c(6,6,6, NA, 23,23, NA,NA,NA,NA))
    expect_equal(sum_by(x, grp = g, na.rm = TRUE, NAopt = FALSE),
                 c(6,6,6, 0, 23,23, 306,306,306,306))
    expect_equal(sum_by(x, grp = g, na.rm = TRUE, NAopt = TRUE),
                 c(6,6,6, NA, 23,23, 306,306,306,306))

    ## with missing, and x integer
    x <- as.integer(x)
    expect_equal(sum_by(x, grp = g, na.rm = FALSE, NAopt = FALSE),
                 c(6,6,6, NA, 23,23, NA,NA,NA,NA))
    expect_equal(sum_by(x, grp = g, na.rm = TRUE, NAopt = FALSE),
                 c(6,6,6, 0, 23,23, 306,306,306,306))
    expect_equal(sum_by(x, grp = g, na.rm = TRUE, NAopt = TRUE),
                 c(6,6,6, NA, 23,23, 306,306,306,306))

    ## lets just check the underlying function:
    expect_equal(sumi_g(x, g = g, na_rm = FALSE, na_opt = FALSE),
                 c(6,6,6, NA, 23,23, NA,NA,NA,NA))
    expect_equal(sumi_g(x, g = g, na_rm = TRUE, na_opt = FALSE),
                 c(6,6,6, 0, 23,23, 306,306,306,306))
    expect_equal(sumi_g(x, g = g, na_rm = TRUE, na_opt = TRUE),
                 c(6,6,6, NA, 23,23, 306,306,306,306))
})

test_that("max_by works", {
    ## standard
    g <- as.integer(c(2,2, 1,1,1, 3,3,3))
    x <- c(-1,0, 0,7,1, 9,1,5)
    expect_equal(max_by(x, grp = g), c(0,0, 7,7,7, 9,9,9))
    ## standard and integer
    expect_equal(max_by(as.integer(x), grp = g), c(0,0, 7,7,7, 9,9,9))


    ## max_by with missing
    g <- as.integer(c(7,7,7, 12, 1,1, 6,6,6,6))
    x <- c(1:3, NA_real_, 11:12, NA_real_,101:103)
    expect_equal(max_by(x, grp = g, na.rm = FALSE, NAopt = FALSE),
                 c(3,3,3, NA, 12,12, NA,NA,NA,NA))
    expect_equal(max_by(x, grp = g, na.rm = TRUE, NAopt = FALSE),
                 c(3,3,3, -Inf, 12,12, 103,103,103,103))
    expect_equal(max_by(x, grp = g, na.rm = TRUE, NAopt = TRUE),
                 c(3,3,3, NA, 12,12, 103,103,103,103))

    ## with missing, and integer
    x <- as.integer(x)
    expect_equal(max_by(x, grp = g, na.rm = FALSE, NAopt = FALSE),
                 c(3,3,3, NA, 12,12, NA,NA,NA,NA))
    expect_equal(max_by(x, grp = g, na.rm = TRUE, NAopt = FALSE),
                 c(3,3,3, -Inf, 12,12, 103,103,103,103))
    expect_equal(max_by(x, grp = g, na.rm = TRUE, NAopt = TRUE),
                 c(3,3,3, NA, 12,12, 103,103,103,103))
})

test_that("min_by works", {
    ## standard
    g <- as.integer(c(2,2, 1,1,1, 3,3,3))
    x <- c(-1,0, 0,7,1, 9,1,5)
    expect_equal(min_by(x, grp = g), c(-1,-1, 0,0,0, 1,1,1))
    ## standard and integer
    expect_equal(min_by(as.integer(x), grp = g), c(-1,-1, 0,0,0, 1,1,1))

    ## min_by with missing
    g <- as.integer(c(7,7,7, 12, 1,1, 6,6,6,6))
    x <- c(1:3, NA_real_, 11:12, NA_real_,101:103)
    expect_equal(min_by(x, grp = g, na.rm = FALSE, NAopt = FALSE),
                 c(1,1,1, NA, 11,11, NA,NA,NA,NA))
    expect_equal(min_by(x, grp = g, na.rm = TRUE, NAopt = FALSE),
                 c(1,1,1, Inf, 11,11, 101,101,101,101))
    expect_equal(min_by(x, grp = g, na.rm = TRUE, NAopt = TRUE),
                 c(1,1,1, NA, 11,11, 101,101,101,101))

    ## with missing, and integer
    x <- as.integer(x)
    expect_equal(min_by(x, grp = g, na.rm = FALSE, NAopt = FALSE),
                 c(1,1,1, NA, 11,11, NA,NA,NA,NA))
    expect_equal(min_by(x, grp = g, na.rm = TRUE, NAopt = FALSE),
                 c(1,1,1, Inf, 11,11, 101,101,101,101))
    expect_equal(min_by(x, grp = g, na.rm = TRUE, NAopt = TRUE),
                 c(1,1,1, NA, 11,11, 101,101,101,101))
})

test_that("n_by works", {
    g <- as.integer(c(7,7,7, 12, 1,1, 6,6,6,6))
    x <- c(1:3, NA_real_, 11:12, NA_real_,101:103)
    expect_equal(n_by(x, grp = g, na.rm = FALSE),
                 c(3,3,3, 1, 2,2, 4,4,4,4))
    expect_equal(n_by(x, grp = g, na.rm = TRUE),
                 c(3,3,3, 0, 2,2, 3,3,3,3))
})

test_that("dup_g works", {
    x <- c(1,2,1,2,2,3,2)
    i <- c(F,F,T,T,T,F,T)
    expect_equal(dup_g(v = x, g = rep(1L, length(x))), i)
    expect_equal(dup_g(v = LETTERS[x], g = rep(1L, length(x))), i)
    g <- as.integer(c(12, 1,1, 6,6,6,6,6,6,6))
    x <- c(NA, 1,NA, 1,NA,2,1,NA,4,NA)
    i1 <- c(F, F,F, F,F,F,T,T,F,T)
    i2 <- c(F, F,F, F,F,F,T,F,F,F)
    expect_equal(dup_g(v = x, g = g, na_rm = FALSE), i1)
    expect_equal(dup_g(v = x, g = g, na_rm = TRUE), i2)
    expect_equal(dup_g(v = LETTERS[x], g = g, na_rm = FALSE), i1)
    expect_equal(dup_g(v = LETTERS[x], g = g, na_rm = TRUE), i2)
})

test_that("uniqueN_by works", {
    g <- as.integer(c(7,7,7, 12, 1,1, 6,6,6,6,6))
    x <- LETTERS[c(3,2,3, NA, 3,1, 3,NA,1,3,NA)]
    ## data.frame(x = x,
    ##            grp = g,
    ##            u1 = uniqueN_by(x, grp = g, na.rm = FALSE),
    ##            u2 = uniqueN_by(x, grp = g, na.rm = TRUE))
    expect_equal(uniqueN_by(x, grp = g, na.rm = FALSE),
                 c(2,2,2, 1, 2,2, 3,3,3,3,3))
    expect_equal(uniqueN_by(x, grp = g, na.rm = TRUE),
                 c(2,2,2, 0, 2,2, 2,2,2,2,2))
})

test_that("anyTRUE_by works", {
    g <- as.integer(c(1,1,2,2,2,3))
    x <- c(F,F,T,F,NA,NA)
    expect_equal(anyTRUE_by(x, g), c(F,F,T,T,T,F))
})

test_that("lag1_by works", {
    g <- LETTERS[c(3,3,3,2,2,4,4,4,4)]
    x <- 1:9
    expect_equal(lag1_by(x, g), c(NA,1:2, NA,4, NA,6:8))
})

test_that("lead1_by works", {
    g <- LETTERS[c(3,3,3,2,2,4,4,4,4)]
    x <- 1:9
    expect_equal(lead1_by(x, g), c(2:3,NA, 5,NA, 7:9,NA))
})
