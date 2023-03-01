test_that("lag1 works", {
    expect_equal(lag1(1:4), c(NA_integer_,1:3))
    expect_equal(lag1(7), NA_real_)
    expect_equal(lag1(integer(0)), integer(0))
})

test_that("lead1 works", {
    expect_equal(lead1(1:4), c(2:4, NA_integer_))
    expect_equal(lead1(7), NA_real_)
    expect_equal(lead1(integer(0)), integer(0))
})

## test_that("grp_check works", {
##     ## is grp_check useful for anything at all?
##     grp_check(LETTERS[1:2])
##     grp_check(c(1, 2, 3))
##     grp_check(c(1, 2.2, 3))
## })

test_that("grp_id works", {
    x <- c(7L, 7L, 1L, 4L, 4L, 4L)
    i <- c(1L, 1L, 2L, 3L, 3L, 3L)
    expect_equal(grp_id(x), x)
    expect_equal(grp_id(LETTERS[x]), i)
    expect_equal(grp_id(factor(x, levels = c(7L, 1L, 4L))), i)
    expect_warning(grp_id(x+0.1))
    suppressWarnings(expect_equal(grp_id(x+0.1), x))
    class(x) <- "foo"
    expect_error(grp_id(x))
})

test_that("grp_n works", {
    x <- c(7L, 7L, 1L, 4L, 4L, 4L)
    i <- c(2L, 2L, 1L, 3L, 3L, 3L)
    expect_equal(grp_n(x), i)
    expect_equal(grp_n(LETTERS[x]), i)
    expect_equal(grp_n(factor(x)), i)
})

test_that("grp_last works", {
    x <- c(7L, 7L, 1L, 4L, 4L, 4L)
    i <- c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)
    expect_equal(grp_last(x), i)
})

test_that("grp_first works", {
    x <- c(7L, 7L, 1L, 4L, 4L, 4L)
    i <- c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
    expect_equal(grp_first(x), i)
})

test_that("grp_row works", {
    x <- c(7L, 7L, 1L, 4L, 4L, 4L)
    i <- c(1:2, 1L, 1:3)
    expect_equal(grp_row(x), i)
})
