context("requirements for [")

test_that("dims are the same", {

    expect_equal(dim(TST_A), dim(TST_B))

})

test_that("dimnames are the same", {

    expect_equal(dimnames(TST_A), dimnames(TST_B))

})

test_that("minimum dimension requirement are met", {

    expect_gt(nrow(TST_A), 3)
    expect_gt(ncol(TST_A), 3)
    expect_gt(prod(dim(TST_A)), 3)

})

test_that("maximum dimension requirement are met", {

    expect_lt(nrow(TST_A), OUT_OF_BOUNDS_INT)
    expect_lt(ncol(TST_A), OUT_OF_BOUNDS_INT)
    expect_lt(prod(dim(TST_A)), OUT_OF_BOUNDS_INT)

})


context("[")

test_subsetting <- function(...) {
    expect_equal(TST_A[...], TST_B[...])
}

test_subsetting_error <- function(...) {
    expect_error(TST_A[...])
    expect_error(TST_B[...])
}

test_that("single indexing by nothing works", {

    test_subsetting()
    test_subsetting(drop = TRUE)
    test_subsetting(drop = FALSE)
})

test_that("single indexing by positive integers works", {

    test_subsetting(1)
    test_subsetting(1, drop = TRUE)
    test_subsetting(1, drop = FALSE)

    test_subsetting(c(1, 2))
    test_subsetting(c(1, 2), drop = TRUE)
    test_subsetting(c(1, 2), drop = FALSE)

    test_subsetting(c(2, 1))
    test_subsetting(c(2, 1), drop = TRUE)
    test_subsetting(c(2, 1), drop = FALSE)

    test_subsetting(1.1)
    test_subsetting(1.1, drop = TRUE)
    test_subsetting(1.1, drop = FALSE)

    test_subsetting(c(1.1, 2.1))
    test_subsetting(c(1.1, 2.1), drop = TRUE)
    test_subsetting(c(1.1, 2.1), drop = FALSE)

    test_subsetting(c(2.1, 1.1))
    test_subsetting(c(2.1, 1.1), drop = TRUE)
    test_subsetting(c(2.1, 1.1), drop = FALSE)

    test_subsetting(1.9)
    test_subsetting(1.9, drop = TRUE)
    test_subsetting(1.9, drop = FALSE)

    test_subsetting(c(1.9, 2.9))
    test_subsetting(c(1.9, 2.9), drop = TRUE)
    test_subsetting(c(1.9, 2.9), drop = FALSE)

    test_subsetting(c(2.9, 1.9))
    test_subsetting(c(2.9, 1.9), drop = TRUE)
    test_subsetting(c(2.9, 1.9), drop = FALSE)

    test_subsetting(OUT_OF_BOUNDS_INT)
    test_subsetting(OUT_OF_BOUNDS_INT, drop = TRUE)
    test_subsetting(OUT_OF_BOUNDS_INT, drop = FALSE)

    test_subsetting(c(OUT_OF_BOUNDS_INT, 2))
    test_subsetting(c(OUT_OF_BOUNDS_INT, 2), drop = TRUE)
    test_subsetting(c(OUT_OF_BOUNDS_INT, 2), drop = FALSE)

    test_subsetting(c(2, OUT_OF_BOUNDS_INT))
    test_subsetting(c(2, OUT_OF_BOUNDS_INT), drop = TRUE)
    test_subsetting(c(2, OUT_OF_BOUNDS_INT), drop = FALSE)

    m <- matrix(data = c(1, 1, 2, 2), ncol = 2, byrow = TRUE)
    test_subsetting(m)
    test_subsetting(m, drop = TRUE)
    test_subsetting(m, drop = FALSE)

    m <- matrix(data = c(1, 1, 2, 2), ncol = 2, byrow = TRUE)
    test_subsetting(m)
    test_subsetting(m, drop = TRUE)
    test_subsetting(m, drop = FALSE)

    m <- matrix(data = c(2, 2, 1, 1), ncol = 2, byrow = TRUE)
    test_subsetting(m)
    test_subsetting(m, drop = TRUE)
    test_subsetting(m, drop = FALSE)

    m <- matrix(data = c(1.1, 1.1, 2.1, 2.1), ncol = 2, byrow = TRUE)
    test_subsetting(m)
    test_subsetting(m, drop = TRUE)
    test_subsetting(m, drop = FALSE)

    m <- matrix(data = c(2.1, 2.1, 1.1, 1.1), ncol = 2, byrow = TRUE)
    test_subsetting(m)
    test_subsetting(m, drop = TRUE)
    test_subsetting(m, drop = FALSE)

    m <- matrix(data = c(1.9, 1.9, 2.9, 2.9), ncol = 2, byrow = TRUE)
    test_subsetting(m)
    test_subsetting(m, drop = TRUE)
    test_subsetting(m, drop = FALSE)

    m <- matrix(data = c(2.9, 2.9, 1.9, 1.9), ncol = 2, byrow = TRUE)
    test_subsetting(m)
    test_subsetting(m, drop = TRUE)
    test_subsetting(m, drop = FALSE)

    m <- matrix(data = c(OUT_OF_BOUNDS_INT, OUT_OF_BOUNDS_INT), ncol = 2, byrow = TRUE)
    test_subsetting_error(m)
    test_subsetting_error(m, drop = TRUE)
    test_subsetting_error(m, drop = FALSE)

    test_subsetting(c(1, NA))
    test_subsetting(c(1, NA), drop = TRUE)
    test_subsetting(c(1, NA), drop = FALSE)

})

test_that("single indexing by negative integers works", {

    test_subsetting(-1)
    test_subsetting(-1, drop = TRUE)
    test_subsetting(-1, drop = FALSE)

    test_subsetting(c(-1, -2))
    test_subsetting(c(-1, -2), drop = TRUE)
    test_subsetting(c(-1, -2), drop = FALSE)

    test_subsetting(c(-2, -1))
    test_subsetting(c(-2, -1), drop = TRUE)
    test_subsetting(c(-2, -1), drop = FALSE)

    test_subsetting(-1.1)
    test_subsetting(-1.1, drop = TRUE)
    test_subsetting(-1.1, drop = FALSE)

    test_subsetting(c(-1.1, -2.1))
    test_subsetting(c(-1.1, -2.1), drop = TRUE)
    test_subsetting(c(-1.1, -2.1), drop = FALSE)

    test_subsetting(c(-2.1, -1.1))
    test_subsetting(c(-2.1, -1.1), drop = TRUE)
    test_subsetting(c(-2.1, -1.1), drop = FALSE)

    test_subsetting(-1.9)
    test_subsetting(-1.9, drop = TRUE)
    test_subsetting(-1.9, drop = FALSE)

    test_subsetting(c(-1.9, -2.9))
    test_subsetting(c(-1.9, -2.9), drop = TRUE)
    test_subsetting(c(-1.9, -2.9), drop = FALSE)

    test_subsetting(c(-2.9, -1.9))
    test_subsetting(c(-2.9, -1.9), drop = TRUE)
    test_subsetting(c(-2.9, -1.9), drop = FALSE)

    test_subsetting(-OUT_OF_BOUNDS_INT)
    test_subsetting(-OUT_OF_BOUNDS_INT, drop = TRUE)
    test_subsetting(-OUT_OF_BOUNDS_INT, drop = FALSE)

    test_subsetting(c(-OUT_OF_BOUNDS_INT, -2))
    test_subsetting(c(-OUT_OF_BOUNDS_INT, -2), drop = TRUE)
    test_subsetting(c(-OUT_OF_BOUNDS_INT, -2), drop = FALSE)

    test_subsetting(c(-2, -OUT_OF_BOUNDS_INT))
    test_subsetting(c(-2, -OUT_OF_BOUNDS_INT), drop = TRUE)
    test_subsetting(c(-2, -OUT_OF_BOUNDS_INT), drop = FALSE)

    test_subsetting_error(c(-1, NA))
    test_subsetting_error(c(-1, NA), drop = TRUE)
    test_subsetting_error(c(-1, NA), drop = FALSE)

})

test_that("single indexing by logicals works", {

    test_subsetting(TRUE)
    test_subsetting(TRUE, drop = TRUE)
    test_subsetting(TRUE, drop = FALSE)

    test_subsetting(FALSE)
    test_subsetting(FALSE, drop = FALSE)
    test_subsetting(FALSE, drop = FALSE)

    test_subsetting(c(TRUE, FALSE))
    test_subsetting(c(TRUE, FALSE), drop = TRUE)
    test_subsetting(c(TRUE, FALSE), drop = FALSE)

    test_subsetting(c(FALSE, TRUE))
    test_subsetting(c(FALSE, TRUE), drop = TRUE)
    test_subsetting(c(FALSE, TRUE), drop = FALSE)

    test_subsetting(rep_len(TRUE, OUT_OF_BOUNDS_INT))
    test_subsetting(rep_len(TRUE, OUT_OF_BOUNDS_INT), drop = rep_len(TRUE, OUT_OF_BOUNDS_INT))
    test_subsetting(rep_len(TRUE, OUT_OF_BOUNDS_INT), drop = FALSE)

    test_subsetting(rep_len(FALSE, OUT_OF_BOUNDS_INT))
    test_subsetting(rep_len(FALSE, OUT_OF_BOUNDS_INT), drop = rep_len(FALSE, OUT_OF_BOUNDS_INT))
    test_subsetting(rep_len(FALSE, OUT_OF_BOUNDS_INT), drop = rep_len(FALSE, OUT_OF_BOUNDS_INT))

    test_subsetting(rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT))
    test_subsetting(rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = TRUE)
    test_subsetting(rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = FALSE)

    test_subsetting(rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT))
    test_subsetting(rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = TRUE)
    test_subsetting(rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = FALSE)

    m <- matrix(data = rnorm(25), nrow = 5, ncol = 5)
    test_subsetting(m > 1)
    test_subsetting(m > 1, drop = TRUE)
    test_subsetting(m > 1, drop = FALSE)

    test_subsetting(c(TRUE, NA))
    test_subsetting(c(TRUE, NA), drop = TRUE)
    test_subsetting(c(TRUE, NA), drop = FALSE)

    test_subsetting(c(FALSE, NA))
    test_subsetting(c(FALSE, NA), drop = TRUE)
    test_subsetting(c(FALSE, NA), drop = FALSE)

})

test_that("single indexing by characters works", {

    if (is.null(dimnames(TST_A))) {
        skip("skipping character indexing because dimnames are NULL")
    }

    ROW_NAME_1 <- rownames(TST_A)[1]
    ROW_NAME_2 <- rownames(TST_A)[2]
    COL_NAME_1 <- colnames(TST_A)[1]
    COL_NAME_2 <- colnames(TST_A)[2]

    test_subsetting(ROW_NAME_1)
    test_subsetting(ROW_NAME_1, drop = TRUE)
    test_subsetting(ROW_NAME_1, drop = FALSE)

    test_subsetting(OUT_OF_BOUNDS_CHAR)
    test_subsetting(OUT_OF_BOUNDS_CHAR, drop = TRUE)
    test_subsetting(OUT_OF_BOUNDS_CHAR, drop = FALSE)

    m <- matrix(data = c(ROW_NAME_1, COL_NAME_1, ROW_NAME_2, COL_NAME_2), ncol = 2, byrow = TRUE)
    test_subsetting(m)
    test_subsetting(m, drop = TRUE)
    test_subsetting(m, drop = FALSE)

    m <- matrix(data = c(ROW_NAME_2, COL_NAME_2, ROW_NAME_1, COL_NAME_1), ncol = 2, byrow = TRUE)
    test_subsetting(m)
    test_subsetting(m, drop = TRUE)
    test_subsetting(m, drop = FALSE)

    m <- matrix(data = c(OUT_OF_BOUNDS_CHAR, OUT_OF_BOUNDS_CHAR), ncol = 2, byrow = TRUE)
    test_subsetting_error(m)
    test_subsetting_error(m, drop = TRUE)
    test_subsetting_error(m, drop = FALSE)

    test_subsetting(c(ROW_NAME_1, NA))
    test_subsetting(c(ROW_NAME_1, NA), drop = TRUE)
    test_subsetting(c(ROW_NAME_1, NA), drop = FALSE)

})

test_that("single indexing by NA works", {

    test_subsetting(NA)
    test_subsetting(NA, drop = TRUE)
    test_subsetting(NA, drop = FALSE)

    test_subsetting(NA_integer_)
    test_subsetting(NA_integer_, drop = TRUE)
    test_subsetting(NA_integer_, drop = FALSE)

    test_subsetting(NA_character_)
    test_subsetting(NA_character_, drop = TRUE)
    test_subsetting(NA_character_, drop = FALSE)

})

test_that("single indexing by zero works", {

    #test_subsetting_error(0) # Not implemented
    #test_subsetting_error(0, drop = TRUE) # Not implemented
    #test_subsetting_error(0, drop = FALSE) # Not implemented

    test_subsetting(c(0, 1))
    test_subsetting(c(0, 1), drop = TRUE)
    test_subsetting(c(0, 1), drop = FALSE)

    test_subsetting(c(0, -1))
    test_subsetting(c(0, -1), drop = TRUE)
    test_subsetting(c(0, -1), drop = FALSE)

    test_subsetting_error(c(0, 1, -1))
    test_subsetting_error(c(0, 1, -1), drop = TRUE)
    test_subsetting_error(c(0, 1, -1), drop = FALSE)

})

test_that("multi indexing by nothing works", {

    test_subsetting(, )
    test_subsetting(, , drop = TRUE)
    test_subsetting(, , drop = FALSE)

})

test_that("multi indexing by positive integers works", {

    test_subsetting(1, )
    test_subsetting(1, , drop = TRUE)
    test_subsetting(1, , drop = FALSE)

    test_subsetting(, 1)
    test_subsetting(, 1, drop = TRUE)
    test_subsetting(, 1, drop = FALSE)

    test_subsetting(1, 1)
    test_subsetting(1, 1, drop = TRUE)
    test_subsetting(1, 1, drop = FALSE)

    test_subsetting(c(1, 2), )
    test_subsetting(c(1, 2), , drop = TRUE)
    test_subsetting(c(1, 2), , drop = FALSE)

    test_subsetting(, c(1, 2))
    test_subsetting(, c(1, 2), drop = TRUE)
    test_subsetting(, c(1, 2), drop = FALSE)

    test_subsetting(c(1, 2), c(1, 2))
    test_subsetting(c(1, 2), c(1, 2), drop = TRUE)
    test_subsetting(c(1, 2), c(1, 2), drop = FALSE)

    test_subsetting(c(2, 1), )
    test_subsetting(c(2, 1), , drop = TRUE)
    test_subsetting(c(2, 1), , drop = FALSE)

    test_subsetting(, c(2, 1))
    test_subsetting(, c(2, 1), drop = TRUE)
    test_subsetting(, c(2, 1), drop = FALSE)

    test_subsetting(c(2, 1), c(2, 1))
    test_subsetting(c(2, 1), c(2, 1), drop = TRUE)
    test_subsetting(c(2, 1), c(2, 1), drop = FALSE)

    test_subsetting(1.1, )
    test_subsetting(1.1, , drop = TRUE)
    test_subsetting(1.1, , drop = FALSE)

    test_subsetting(, 1.1)
    test_subsetting(, 1.1, drop = TRUE)
    test_subsetting(, 1.1, drop = FALSE)

    test_subsetting(1.1, 1.1)
    test_subsetting(1.1, 1.1, drop = TRUE)
    test_subsetting(1.1, 1.1, drop = FALSE)

    test_subsetting(c(1.1, 2.1), )
    test_subsetting(c(1.1, 2.1), , drop = TRUE)
    test_subsetting(c(1.1, 2.1), , drop = FALSE)

    test_subsetting(, c(1.1, 2.1))
    test_subsetting(, c(1.1, 2.1), drop = TRUE)
    test_subsetting(, c(1.1, 2.1), drop = FALSE)

    test_subsetting(c(1.1, 2.1), c(1.1, 2.1))
    test_subsetting(c(1.1, 2.1), c(1.1, 2.1), drop = TRUE)
    test_subsetting(c(1.1, 2.1), c(1.1, 2.1), drop = FALSE)

    test_subsetting(c(2.1, 1.1), )
    test_subsetting(c(2.1, 1.1), , drop = TRUE)
    test_subsetting(c(2.1, 1.1), , drop = FALSE)

    test_subsetting(, c(2.1, 1.1))
    test_subsetting(, c(2.1, 1.1), drop = TRUE)
    test_subsetting(, c(2.1, 1.1), drop = FALSE)

    test_subsetting(c(2.1, 1.1), c(2.1, 1.1))
    test_subsetting(c(2.1, 1.1), c(2.1, 1.1), drop = TRUE)
    test_subsetting(c(2.1, 1.1), c(2.1, 1.1), drop = FALSE)

    test_subsetting(1.9, )
    test_subsetting(1.9, , drop = TRUE)
    test_subsetting(1.9, , drop = FALSE)

    test_subsetting(, 1.9)
    test_subsetting(, 1.9, drop = TRUE)
    test_subsetting(, 1.9, drop = FALSE)

    test_subsetting(1.9, 1.9)
    test_subsetting(1.9, 1.9, drop = TRUE)
    test_subsetting(1.9, 1.9, drop = FALSE)

    test_subsetting(c(1.9, 2.9), )
    test_subsetting(c(1.9, 2.9), , drop = TRUE)
    test_subsetting(c(1.9, 2.9), , drop = FALSE)

    test_subsetting(, c(1.9, 2.9))
    test_subsetting(, c(1.9, 2.9), drop = TRUE)
    test_subsetting(, c(1.9, 2.9), drop = FALSE)

    test_subsetting(c(1.9, 2.9), c(1.9, 2.9))
    test_subsetting(c(1.9, 2.9), c(1.9, 2.9), drop = TRUE)
    test_subsetting(c(1.9, 2.9), c(1.9, 2.9), drop = FALSE)

    test_subsetting(c(2.9, 1.9), )
    test_subsetting(c(2.9, 1.9), , drop = TRUE)
    test_subsetting(c(2.9, 1.9), , drop = FALSE)

    test_subsetting(, c(2.9, 1.9))
    test_subsetting(, c(2.9, 1.9), drop = TRUE)
    test_subsetting(, c(2.9, 1.9), drop = FALSE)

    test_subsetting(c(2.9, 1.9), c(2.9, 1.9))
    test_subsetting(c(2.9, 1.9), c(2.9, 1.9), drop = TRUE)
    test_subsetting(c(2.9, 1.9), c(2.9, 1.9), drop = FALSE)

    test_subsetting_error(OUT_OF_BOUNDS_INT, )
    test_subsetting_error(OUT_OF_BOUNDS_INT, , drop = TRUE)
    test_subsetting_error(OUT_OF_BOUNDS_INT, , drop = FALSE)

    test_subsetting_error(, OUT_OF_BOUNDS_INT)
    test_subsetting_error(, OUT_OF_BOUNDS_INT, drop = TRUE)
    test_subsetting_error(, OUT_OF_BOUNDS_INT, drop = FALSE)

    test_subsetting_error(OUT_OF_BOUNDS_INT, OUT_OF_BOUNDS_INT)
    test_subsetting_error(OUT_OF_BOUNDS_INT, OUT_OF_BOUNDS_INT, drop = TRUE)
    test_subsetting_error(OUT_OF_BOUNDS_INT, OUT_OF_BOUNDS_INT, drop = FALSE)

    test_subsetting_error(c(OUT_OF_BOUNDS_INT, 2), )
    test_subsetting_error(c(OUT_OF_BOUNDS_INT, 2), , drop = TRUE)
    test_subsetting_error(c(OUT_OF_BOUNDS_INT, 2), , drop = FALSE)

    test_subsetting_error(, c(OUT_OF_BOUNDS_INT, 2))
    test_subsetting_error(, c(OUT_OF_BOUNDS_INT, 2), drop = TRUE)
    test_subsetting_error(, c(OUT_OF_BOUNDS_INT, 2), drop = FALSE)

    test_subsetting_error(c(OUT_OF_BOUNDS_INT, 2), c(OUT_OF_BOUNDS_INT, 2))
    test_subsetting_error(c(OUT_OF_BOUNDS_INT, 2), c(OUT_OF_BOUNDS_INT, 2), drop = TRUE)
    test_subsetting_error(c(OUT_OF_BOUNDS_INT, 2), c(OUT_OF_BOUNDS_INT, 2), drop = FALSE)

    test_subsetting_error(c(2, OUT_OF_BOUNDS_INT), )
    test_subsetting_error(c(2, OUT_OF_BOUNDS_INT), , drop = TRUE)
    test_subsetting_error(c(2, OUT_OF_BOUNDS_INT), , drop = FALSE)

    test_subsetting_error(, c(2, OUT_OF_BOUNDS_INT))
    test_subsetting_error(, c(2, OUT_OF_BOUNDS_INT), drop = TRUE)
    test_subsetting_error(, c(2, OUT_OF_BOUNDS_INT), drop = FALSE)

    test_subsetting_error(c(2, OUT_OF_BOUNDS_INT), c(2, OUT_OF_BOUNDS_INT))
    test_subsetting_error(c(2, OUT_OF_BOUNDS_INT), c(2, OUT_OF_BOUNDS_INT), drop = TRUE)
    test_subsetting_error(c(2, OUT_OF_BOUNDS_INT), c(2, OUT_OF_BOUNDS_INT), drop = FALSE)

    test_subsetting(c(1, NA), )
    test_subsetting(c(1, NA), , drop = TRUE)
    test_subsetting(c(1, NA), , drop = FALSE)

    test_subsetting(, c(1, NA))
    test_subsetting(, c(1, NA), drop = TRUE)
    test_subsetting(, c(1, NA), drop = FALSE)

    test_subsetting(c(1, NA), c(1, NA))
    test_subsetting(c(1, NA), c(1, NA), drop = TRUE)
    test_subsetting(c(1, NA), c(1, NA), drop = FALSE)

})

test_that("multi indexing by negative integers works", {

    test_subsetting(c(-1), )
    test_subsetting(c(-1), , drop = TRUE)
    test_subsetting(c(-1), , drop = FALSE)

    test_subsetting(, c(-1))
    test_subsetting(, c(-1), drop = TRUE)
    test_subsetting(, c(-1), drop = FALSE)

    test_subsetting(c(-1), c(-1))
    test_subsetting(c(-1), c(-1), drop = TRUE)
    test_subsetting(c(-1), c(-1), drop = FALSE)

    test_subsetting(c(-1, -2), )
    test_subsetting(c(-1, -2), , drop = TRUE)
    test_subsetting(c(-1, -2), , drop = FALSE)

    test_subsetting(, c(-1, -2))
    test_subsetting(, c(-1, -2), drop = TRUE)
    test_subsetting(, c(-1, -2), drop = FALSE)

    test_subsetting(c(-1, -2), c(-1, -2))
    test_subsetting(c(-1, -2), c(-1, -2), drop = TRUE)
    test_subsetting(c(-1, -2), c(-1, -2), drop = FALSE)

    test_subsetting(c(-2, -1), )
    test_subsetting(c(-2, -1), , drop = TRUE)
    test_subsetting(c(-2, -1), , drop = FALSE)

    test_subsetting(, c(-2, -1))
    test_subsetting(, c(-2, -1), drop = TRUE)
    test_subsetting(, c(-2, -1), drop = FALSE)

    test_subsetting(c(-2, -1), c(-2, -1))
    test_subsetting(c(-2, -1), c(-2, -1), drop = TRUE)
    test_subsetting(c(-2, -1), c(-2, -1), drop = FALSE)

    test_subsetting(c(-1.1), )
    test_subsetting(c(-1.1), , drop = TRUE)
    test_subsetting(c(-1.1), , drop = FALSE)

    test_subsetting(, c(-1.1))
    test_subsetting(, c(-1.1), drop = TRUE)
    test_subsetting(, c(-1.1), drop = FALSE)

    test_subsetting(c(-1.1), c(-1.1))
    test_subsetting(c(-1.1), c(-1.1), drop = TRUE)
    test_subsetting(c(-1.1), c(-1.1), drop = FALSE)

    test_subsetting(c(-1.1, -2.1), )
    test_subsetting(c(-1.1, -2.1), , drop = TRUE)
    test_subsetting(c(-1.1, -2.1), , drop = FALSE)

    test_subsetting(, c(-1.1, -2.1))
    test_subsetting(, c(-1.1, -2.1), drop = TRUE)
    test_subsetting(, c(-1.1, -2.1), drop = FALSE)

    test_subsetting(c(-1.1, -2.1), c(-1.1, -2.1))
    test_subsetting(c(-1.1, -2.1), c(-1.1, -2.1), drop = TRUE)
    test_subsetting(c(-1.1, -2.1), c(-1.1, -2.1), drop = FALSE)

    test_subsetting(c(-2.1, -1.1), )
    test_subsetting(c(-2.1, -1.1), , drop = TRUE)
    test_subsetting(c(-2.1, -1.1), , drop = FALSE)

    test_subsetting(, c(-2.1, -1.1))
    test_subsetting(, c(-2.1, -1.1), drop = TRUE)
    test_subsetting(, c(-2.1, -1.1), drop = FALSE)

    test_subsetting(c(-2.1, -1.1), c(-2.1, -1.1))
    test_subsetting(c(-2.1, -1.1), c(-2.1, -1.1), drop = TRUE)
    test_subsetting(c(-2.1, -1.1), c(-2.1, -1.1), drop = FALSE)

    test_subsetting(c(-1.9), )
    test_subsetting(c(-1.9), , drop = TRUE)
    test_subsetting(c(-1.9), , drop = FALSE)

    test_subsetting(, c(-1.9))
    test_subsetting(, c(-1.9), drop = TRUE)
    test_subsetting(, c(-1.9), drop = FALSE)

    test_subsetting(c(-1.9), c(-1.9))
    test_subsetting(c(-1.9), c(-1.9), drop = TRUE)
    test_subsetting(c(-1.9), c(-1.9), drop = FALSE)

    test_subsetting(c(-1.9, -2.9), )
    test_subsetting(c(-1.9, -2.9), , drop = TRUE)
    test_subsetting(c(-1.9, -2.9), , drop = FALSE)

    test_subsetting(, c(-1.9, -2.9))
    test_subsetting(, c(-1.9, -2.9), drop = TRUE)
    test_subsetting(, c(-1.9, -2.9), drop = FALSE)

    test_subsetting(c(-1.9, -2.9), c(-1.9, -2.9))
    test_subsetting(c(-1.9, -2.9), c(-1.9, -2.9), drop = TRUE)
    test_subsetting(c(-1.9, -2.9), c(-1.9, -2.9), drop = FALSE)

    test_subsetting(c(-2.9, -1.9), )
    test_subsetting(c(-2.9, -1.9), , drop = TRUE)
    test_subsetting(c(-2.9, -1.9), , drop = FALSE)

    test_subsetting(, c(-2.9, -1.9))
    test_subsetting(, c(-2.9, -1.9), drop = TRUE)
    test_subsetting(, c(-2.9, -1.9), drop = FALSE)

    test_subsetting(c(-2.9, -1.9), c(-2.9, -1.9))
    test_subsetting(c(-2.9, -1.9), c(-2.9, -1.9), drop = TRUE)
    test_subsetting(c(-2.9, -1.9), c(-2.9, -1.9), drop = FALSE)

    test_subsetting(c(-OUT_OF_BOUNDS_INT), )
    test_subsetting(c(-OUT_OF_BOUNDS_INT), , drop = TRUE)
    test_subsetting(c(-OUT_OF_BOUNDS_INT), , drop = FALSE)

    test_subsetting(, c(-OUT_OF_BOUNDS_INT))
    test_subsetting(, c(-OUT_OF_BOUNDS_INT), drop = TRUE)
    test_subsetting(, c(-OUT_OF_BOUNDS_INT), drop = FALSE)

    test_subsetting(c(-OUT_OF_BOUNDS_INT), c(-OUT_OF_BOUNDS_INT))
    test_subsetting(c(-OUT_OF_BOUNDS_INT), c(-OUT_OF_BOUNDS_INT), drop = TRUE)
    test_subsetting(c(-OUT_OF_BOUNDS_INT), c(-OUT_OF_BOUNDS_INT), drop = FALSE)

    test_subsetting(c(-OUT_OF_BOUNDS_INT, -2), )
    test_subsetting(c(-OUT_OF_BOUNDS_INT, -2), , drop = TRUE)
    test_subsetting(c(-OUT_OF_BOUNDS_INT, -2), , drop = FALSE)

    test_subsetting(, c(-OUT_OF_BOUNDS_INT, -2))
    test_subsetting(, c(-OUT_OF_BOUNDS_INT, -2), drop = TRUE)
    test_subsetting(, c(-OUT_OF_BOUNDS_INT, -2), drop = FALSE)

    test_subsetting(c(-OUT_OF_BOUNDS_INT, -2), c(-OUT_OF_BOUNDS_INT, -2))
    test_subsetting(c(-OUT_OF_BOUNDS_INT, -2), c(-OUT_OF_BOUNDS_INT, -2), drop = TRUE)
    test_subsetting(c(-OUT_OF_BOUNDS_INT, -2), c(-OUT_OF_BOUNDS_INT, -2), drop = FALSE)

    test_subsetting(c(-2, -OUT_OF_BOUNDS_INT), )
    test_subsetting(c(-2, -OUT_OF_BOUNDS_INT), , drop = TRUE)
    test_subsetting(c(-2, -OUT_OF_BOUNDS_INT), , drop = FALSE)

    test_subsetting(, c(-2, -OUT_OF_BOUNDS_INT))
    test_subsetting(, c(-2, -OUT_OF_BOUNDS_INT), drop = TRUE)
    test_subsetting(, c(-2, -OUT_OF_BOUNDS_INT), drop = FALSE)

    test_subsetting(c(-2, -OUT_OF_BOUNDS_INT), c(-2, -OUT_OF_BOUNDS_INT))
    test_subsetting(c(-2, -OUT_OF_BOUNDS_INT), c(-2, -OUT_OF_BOUNDS_INT), drop = TRUE)
    test_subsetting(c(-2, -OUT_OF_BOUNDS_INT), c(-2, -OUT_OF_BOUNDS_INT), drop = FALSE)

    test_subsetting_error(c(-1, NA), )
    test_subsetting_error(c(-1, NA), , drop = TRUE)
    test_subsetting_error(c(-1, NA), , drop = FALSE)

    test_subsetting_error(, c(-1, NA))
    test_subsetting_error(, c(-1, NA), drop = TRUE)
    test_subsetting_error(, c(-1, NA), drop = FALSE)

    test_subsetting_error(c(-1, NA), c(-1, NA))
    test_subsetting_error(c(-1, NA), c(-1, NA), drop = TRUE)
    test_subsetting_error(c(-1, NA), c(-1, NA), drop = FALSE)

})

test_that("multi indexing by logicals works", {

    test_subsetting(c(TRUE), )
    test_subsetting(c(TRUE), , drop = TRUE)
    test_subsetting(c(TRUE), , drop = FALSE)

    test_subsetting(, c(TRUE))
    test_subsetting(, c(TRUE), drop = TRUE)
    test_subsetting(, c(TRUE), drop = FALSE)

    test_subsetting(c(TRUE), c(TRUE))
    test_subsetting(c(TRUE), c(TRUE), drop = TRUE)
    test_subsetting(c(TRUE), c(TRUE), drop = FALSE)

    test_subsetting_error(c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), )
    test_subsetting_error(c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), , drop = rep_len(TRUE, OUT_OF_BOUNDS_INT))
    test_subsetting_error(c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), , drop = FALSE)

    test_subsetting_error(, c(rep_len(TRUE, OUT_OF_BOUNDS_INT)))
    test_subsetting_error(, c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), drop = rep_len(TRUE, OUT_OF_BOUNDS_INT))
    test_subsetting_error(, c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), drop = FALSE)

    test_subsetting_error(c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), c(rep_len(TRUE, OUT_OF_BOUNDS_INT)))
    test_subsetting_error(c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), drop = rep_len(TRUE, OUT_OF_BOUNDS_INT))
    test_subsetting_error(c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), drop = FALSE)

    test_subsetting(c(FALSE), )
    test_subsetting(c(FALSE), , drop = FALSE)
    test_subsetting(c(FALSE), , drop = FALSE)

    test_subsetting(, c(FALSE))
    test_subsetting(, c(FALSE), drop = FALSE)
    test_subsetting(, c(FALSE), drop = FALSE)

    test_subsetting(c(FALSE), c(FALSE))
    test_subsetting(c(FALSE), c(FALSE), drop = FALSE)
    test_subsetting(c(FALSE), c(FALSE), drop = FALSE)

    test_subsetting_error(c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), )
    test_subsetting_error(c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), , drop = rep_len(FALSE, OUT_OF_BOUNDS_INT))
    test_subsetting_error(c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), , drop = rep_len(FALSE, OUT_OF_BOUNDS_INT))

    test_subsetting_error(, c(rep_len(FALSE, OUT_OF_BOUNDS_INT)))
    test_subsetting_error(, c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), drop = rep_len(FALSE, OUT_OF_BOUNDS_INT))
    test_subsetting_error(, c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), drop = rep_len(FALSE, OUT_OF_BOUNDS_INT))

    test_subsetting_error(c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), c(rep_len(FALSE, OUT_OF_BOUNDS_INT)))
    test_subsetting_error(c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), drop = rep_len(FALSE, OUT_OF_BOUNDS_INT))
    test_subsetting_error(c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), drop = rep_len(FALSE, OUT_OF_BOUNDS_INT))

    test_subsetting(c(TRUE, FALSE), )
    test_subsetting(c(TRUE, FALSE), , drop = TRUE)
    test_subsetting(c(TRUE, FALSE), , drop = FALSE)

    test_subsetting(, c(TRUE, FALSE))
    test_subsetting(, c(TRUE, FALSE), drop = TRUE)
    test_subsetting(, c(TRUE, FALSE), drop = FALSE)

    test_subsetting(c(TRUE, FALSE), c(TRUE, FALSE))
    test_subsetting(c(TRUE, FALSE), c(TRUE, FALSE), drop = TRUE)
    test_subsetting(c(TRUE, FALSE), c(TRUE, FALSE), drop = FALSE)

    test_subsetting_error(rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), )
    test_subsetting_error(rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), , drop = TRUE)
    test_subsetting_error(rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), , drop = FALSE)

    test_subsetting_error(, rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT))
    test_subsetting_error(, rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = TRUE)
    test_subsetting_error(, rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = FALSE)
    expect_error(TST_B[, rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = FALSE])

    test_subsetting_error(rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT))
    test_subsetting_error(rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = TRUE)
    test_subsetting_error(rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = FALSE)
    expect_error(TST_B[rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = FALSE])

    test_subsetting(c(FALSE, TRUE), )
    test_subsetting(c(FALSE, TRUE), , drop = TRUE)
    test_subsetting(c(FALSE, TRUE), , drop = FALSE)

    test_subsetting(, c(FALSE, TRUE))
    test_subsetting(, c(FALSE, TRUE), drop = TRUE)
    test_subsetting(, c(FALSE, TRUE), drop = FALSE)

    test_subsetting(c(FALSE, TRUE), c(FALSE, TRUE))
    test_subsetting(c(FALSE, TRUE), c(FALSE, TRUE), drop = TRUE)
    test_subsetting(c(FALSE, TRUE), c(FALSE, TRUE), drop = FALSE)

    test_subsetting_error(rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), )
    test_subsetting_error(rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), , drop = TRUE)
    test_subsetting_error(rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), , drop = FALSE)
    expect_error(TST_B[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), , drop = FALSE])

    test_subsetting_error(, rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT))
    test_subsetting_error(, rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = TRUE)
    test_subsetting_error(, rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = FALSE)
    expect_error(TST_B[, rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = FALSE])

    test_subsetting_error(rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT))
    test_subsetting_error(rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = TRUE)
    test_subsetting_error(rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = FALSE)
    expect_error(TST_B[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = FALSE])

    test_subsetting(c(TRUE, NA), )
    test_subsetting(c(TRUE, NA), , drop = TRUE)
    test_subsetting(c(TRUE, NA), , drop = FALSE)

    test_subsetting(, c(TRUE, NA))
    test_subsetting(, c(TRUE, NA), drop = TRUE)
    test_subsetting(, c(TRUE, NA), drop = FALSE)

    test_subsetting(c(TRUE, NA), c(TRUE, NA))
    test_subsetting(c(TRUE, NA), c(TRUE, NA), drop = TRUE)
    test_subsetting(c(TRUE, NA), c(TRUE, NA), drop = FALSE)

    test_subsetting(c(FALSE, NA), )
    test_subsetting(c(FALSE, NA), , drop = FALSE)
    test_subsetting(c(FALSE, NA), , drop = FALSE)

    test_subsetting(, c(FALSE, NA))
    test_subsetting(, c(FALSE, NA), drop = FALSE)
    test_subsetting(, c(FALSE, NA), drop = FALSE)

    test_subsetting(c(FALSE, NA), c(FALSE, NA))
    test_subsetting(c(FALSE, NA), c(FALSE, NA), drop = FALSE)
    test_subsetting(c(FALSE, NA), c(FALSE, NA), drop = FALSE)

})

test_that("multi indexing by characters works", {

    if (is.null(dimnames(TST_A))) {
        skip("skipping character indexing because dimnames are NULL")
    }

    ROW_NAME_1 <- rownames(TST_A)[1]
    ROW_NAME_2 <- rownames(TST_A)[2]
    COL_NAME_1 <- colnames(TST_A)[1]
    COL_NAME_2 <- colnames(TST_A)[2]

    test_subsetting(ROW_NAME_1, )
    test_subsetting(ROW_NAME_1, , drop = TRUE)
    test_subsetting(ROW_NAME_1, , drop = FALSE)

    test_subsetting(, COL_NAME_1)
    test_subsetting(, COL_NAME_1, drop = TRUE)
    test_subsetting(, COL_NAME_1, drop = FALSE)

    test_subsetting(ROW_NAME_1, COL_NAME_1)
    test_subsetting(ROW_NAME_1, COL_NAME_1, drop = TRUE)
    test_subsetting(ROW_NAME_1, COL_NAME_1, drop = FALSE)

    test_subsetting_error(OUT_OF_BOUNDS_CHAR, )
    test_subsetting_error(OUT_OF_BOUNDS_CHAR, , drop = TRUE)
    test_subsetting_error(OUT_OF_BOUNDS_CHAR, , drop = FALSE)
    expect_error(TST_B[OUT_OF_BOUNDS_CHAR, , drop = FALSE])

    test_subsetting_error(, OUT_OF_BOUNDS_CHAR)
    test_subsetting_error(, OUT_OF_BOUNDS_CHAR, drop = TRUE)
    test_subsetting_error(, OUT_OF_BOUNDS_CHAR, drop = FALSE)
    expect_error(TST_B[, OUT_OF_BOUNDS_CHAR, drop = FALSE])

    test_subsetting_error(OUT_OF_BOUNDS_CHAR, OUT_OF_BOUNDS_CHAR)
    test_subsetting_error(OUT_OF_BOUNDS_CHAR, OUT_OF_BOUNDS_CHAR, drop = TRUE)
    test_subsetting_error(OUT_OF_BOUNDS_CHAR, OUT_OF_BOUNDS_CHAR, drop = FALSE)
    expect_error(TST_B[OUT_OF_BOUNDS_CHAR, OUT_OF_BOUNDS_CHAR, drop = FALSE])

    test_subsetting(c(ROW_NAME_1, ROW_NAME_2), )
    test_subsetting(c(ROW_NAME_1, ROW_NAME_2), , drop = TRUE)
    test_subsetting(c(ROW_NAME_1, ROW_NAME_2), , drop = FALSE)

    test_subsetting(, c(COL_NAME_1, COL_NAME_2))
    test_subsetting(, c(COL_NAME_1, COL_NAME_2), drop = TRUE)
    test_subsetting(, c(COL_NAME_1, COL_NAME_2), drop = FALSE)

    test_subsetting(c(ROW_NAME_1, ROW_NAME_2), c(COL_NAME_1, COL_NAME_2))
    test_subsetting(c(ROW_NAME_1, ROW_NAME_2), c(COL_NAME_1, COL_NAME_2), drop = TRUE)
    test_subsetting(c(ROW_NAME_1, ROW_NAME_2), c(COL_NAME_1, COL_NAME_2), drop = FALSE)

    test_subsetting(c(ROW_NAME_2, ROW_NAME_1), )
    test_subsetting(c(ROW_NAME_2, ROW_NAME_1), , drop = TRUE)
    test_subsetting(c(ROW_NAME_2, ROW_NAME_1), , drop = FALSE)

    test_subsetting(, c(COL_NAME_2, COL_NAME_1))
    test_subsetting(, c(COL_NAME_2, COL_NAME_1), drop = TRUE)
    test_subsetting(, c(COL_NAME_2, COL_NAME_1), drop = FALSE)

    test_subsetting(c(ROW_NAME_2, ROW_NAME_1), c(COL_NAME_2, COL_NAME_1))
    test_subsetting(c(ROW_NAME_2, ROW_NAME_1), c(COL_NAME_2, COL_NAME_1), drop = TRUE)
    test_subsetting(c(ROW_NAME_2, ROW_NAME_1), c(COL_NAME_2, COL_NAME_1), drop = FALSE)

    test_subsetting_error(c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), )
    test_subsetting_error(c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), , drop = TRUE)
    test_subsetting_error(c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), , drop = FALSE)
    expect_error(TST_B[c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), , drop = FALSE])

    test_subsetting_error(, c(COL_NAME_1, OUT_OF_BOUNDS_CHAR))
    test_subsetting_error(, c(COL_NAME_1, OUT_OF_BOUNDS_CHAR), drop = TRUE)
    test_subsetting_error(, c(COL_NAME_1, OUT_OF_BOUNDS_CHAR), drop = FALSE)
    expect_error(TST_B[, c(COL_NAME_1, OUT_OF_BOUNDS_CHAR), drop = FALSE])

    test_subsetting_error(c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), c(COL_NAME_1, OUT_OF_BOUNDS_CHAR))
    test_subsetting_error(c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), c(COL_NAME_1, OUT_OF_BOUNDS_CHAR), drop = TRUE)
    test_subsetting_error(c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), c(COL_NAME_1, OUT_OF_BOUNDS_CHAR), drop = FALSE)
    expect_error(TST_B[c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), c(COL_NAME_1, OUT_OF_BOUNDS_CHAR), drop = FALSE])

    test_subsetting_error(c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), )
    test_subsetting_error(c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), , drop = TRUE)
    test_subsetting_error(c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), , drop = FALSE)
    expect_error(TST_B[c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), , drop = FALSE])

    test_subsetting_error(, c(OUT_OF_BOUNDS_CHAR, COL_NAME_1))
    test_subsetting_error(, c(OUT_OF_BOUNDS_CHAR, COL_NAME_1), drop = TRUE)
    test_subsetting_error(, c(OUT_OF_BOUNDS_CHAR, COL_NAME_1), drop = FALSE)
    expect_error(TST_B[, c(OUT_OF_BOUNDS_CHAR, COL_NAME_1), drop = FALSE])

    test_subsetting_error(c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), c(OUT_OF_BOUNDS_CHAR, COL_NAME_1))
    test_subsetting_error(c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), c(OUT_OF_BOUNDS_CHAR, COL_NAME_1), drop = TRUE)
    test_subsetting_error(c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), c(OUT_OF_BOUNDS_CHAR, COL_NAME_1), drop = FALSE)
    expect_error(TST_B[c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), c(OUT_OF_BOUNDS_CHAR, COL_NAME_1), drop = FALSE])

    test_subsetting_error(c(ROW_NAME_1, NA), )
    test_subsetting_error(c(ROW_NAME_1, NA), , drop = TRUE)
    test_subsetting_error(c(ROW_NAME_1, NA), , drop = FALSE)
    expect_error(TST_B[c(ROW_NAME_1, NA), , drop = FALSE])

    test_subsetting_error(, c(COL_NAME_1, NA))
    test_subsetting_error(, c(COL_NAME_1, NA), drop = TRUE)
    test_subsetting_error(, c(COL_NAME_1, NA), drop = FALSE)
    expect_error(TST_B[, c(COL_NAME_1, NA), drop = FALSE])

    test_subsetting_error(c(ROW_NAME_1, NA), c(COL_NAME_1, NA))
    test_subsetting_error(c(ROW_NAME_1, NA), c(COL_NAME_1, NA), drop = TRUE)
    test_subsetting_error(c(ROW_NAME_1, NA), c(COL_NAME_1, NA), drop = FALSE)
    expect_error(TST_B[c(ROW_NAME_1, NA), c(COL_NAME_1, NA), drop = FALSE])

})

test_that("multi indexing by NA works", {

    test_subsetting(NA, )
    test_subsetting(NA, , drop = TRUE)
    test_subsetting(NA, , drop = FALSE)

    test_subsetting(, NA)
    test_subsetting(, NA, drop = TRUE)
    test_subsetting(, NA, drop = FALSE)

    test_subsetting(NA, NA)
    test_subsetting(NA, NA, drop = TRUE)
    test_subsetting(NA, NA, drop = FALSE)

    test_subsetting(NA_integer_, )
    test_subsetting(NA_integer_, , drop = TRUE)
    test_subsetting(NA_integer_, , drop = FALSE)

    test_subsetting(, NA_integer_)
    test_subsetting(, NA_integer_, drop = TRUE)
    test_subsetting(, NA_integer_, drop = FALSE)

    test_subsetting(NA_integer_, NA_integer_)
    test_subsetting(NA_integer_, NA_integer_, drop = TRUE)
    test_subsetting(NA_integer_, NA_integer_, drop = FALSE)

    test_subsetting_error(NA_character_, )
    test_subsetting_error(NA_character_, , drop = TRUE)
    test_subsetting_error(NA_character_, , drop = FALSE)
    expect_error(TST_B[NA_character_, , drop = FALSE])

    test_subsetting_error(, NA_character_)
    test_subsetting_error(, NA_character_, drop = TRUE)
    test_subsetting_error(, NA_character_, drop = FALSE)
    expect_error(TST_B[, NA_character_, drop = FALSE])

    test_subsetting_error(NA_character_, NA_character_)
    test_subsetting_error(NA_character_, NA_character_, drop = TRUE)
    test_subsetting_error(NA_character_, NA_character_, drop = FALSE)
    expect_error(TST_B[NA_character_, NA_character_, drop = FALSE])

})

test_that("multi indexing by zero works", {

    #test_subsetting_error(0, 0) # Not implemented
    #test_subsetting_error(0, 0, drop = TRUE) # Not implemented
    #test_subsetting_error(0, 0, drop = FALSE) # Not implemented

    test_subsetting(c(0, 1), )
    test_subsetting(c(0, 1), , drop = TRUE)
    test_subsetting(c(0, 1), , drop = FALSE)

    test_subsetting(, c(0, 1))
    test_subsetting(, c(0, 1), drop = TRUE)
    test_subsetting(, c(0, 1), drop = FALSE)

    test_subsetting(c(0, 1), c(0, 1))
    test_subsetting(c(0, 1), c(0, 1), drop = TRUE)
    test_subsetting(c(0, 1), c(0, 1), drop = FALSE)

    test_subsetting(c(0, -1), )
    test_subsetting(c(0, -1), , drop = TRUE)
    test_subsetting(c(0, -1), , drop = FALSE)

    test_subsetting(, c(0, -1))
    test_subsetting(, c(0, -1), drop = TRUE)
    test_subsetting(, c(0, -1), drop = FALSE)

    test_subsetting(c(0, -1), c(0, -1))
    test_subsetting(c(0, -1), c(0, -1), drop = TRUE)
    test_subsetting(c(0, -1), c(0, -1), drop = FALSE)

    test_subsetting_error(c(0, 1, -1), )
    test_subsetting_error(c(0, 1, -1), , drop = TRUE)
    test_subsetting_error(c(0, 1, -1), , drop = FALSE)
    expect_error(TST_B[c(0, 1, -1), , drop = FALSE])

    test_subsetting_error(, c(0, 1, -1))
    test_subsetting_error(, c(0, 1, -1), drop = TRUE)
    test_subsetting_error(, c(0, 1, -1), drop = FALSE)

    test_subsetting_error(c(0, 1, -1), c(0, 1, -1))
    test_subsetting_error(c(0, 1, -1), c(0, 1, -1), drop = TRUE)
    test_subsetting_error(c(0, 1, -1), c(0, 1, -1), drop = FALSE)

})
