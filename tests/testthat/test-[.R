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

test_that("single indexing by nothing works", {

    expect_equal(TST_A[], TST_B[])
    expect_equal(TST_A[drop = TRUE], TST_B[drop = TRUE])
    expect_equal(TST_A[drop = FALSE], TST_B[drop = FALSE])
})

test_that("single indexing by positive integers works", {

    expect_equal(TST_A[1], TST_B[1])
    expect_equal(TST_A[1, drop = TRUE], TST_B[1, drop = TRUE])
    expect_equal(TST_A[1, drop = FALSE], TST_B[1, drop = FALSE])

    expect_equal(TST_A[c(1, 2)], TST_B[c(1, 2)])
    expect_equal(TST_A[c(1, 2), drop = TRUE], TST_B[c(1, 2), drop = TRUE])
    expect_equal(TST_A[c(1, 2), drop = FALSE], TST_B[c(1, 2), drop = FALSE])

    expect_equal(TST_A[c(2, 1)], TST_B[c(2, 1)])
    expect_equal(TST_A[c(2, 1), drop = TRUE], TST_B[c(2, 1), drop = TRUE])
    expect_equal(TST_A[c(2, 1), drop = FALSE], TST_B[c(2, 1), drop = FALSE])

    expect_equal(TST_A[1.1], TST_B[1.1])
    expect_equal(TST_A[1.1, drop = TRUE], TST_B[1.1, drop = TRUE])
    expect_equal(TST_A[1.1, drop = FALSE], TST_B[1.1, drop = FALSE])

    expect_equal(TST_A[c(1.1, 2.1)], TST_B[c(1.1, 2.1)])
    expect_equal(TST_A[c(1.1, 2.1), drop = TRUE], TST_B[c(1.1, 2.1), drop = TRUE])
    expect_equal(TST_A[c(1.1, 2.1), drop = FALSE], TST_B[c(1.1, 2.1), drop = FALSE])

    expect_equal(TST_A[c(2.1, 1.1)], TST_B[c(2.1, 1.1)])
    expect_equal(TST_A[c(2.1, 1.1), drop = TRUE], TST_B[c(2.1, 1.1), drop = TRUE])
    expect_equal(TST_A[c(2.1, 1.1), drop = FALSE], TST_B[c(2.1, 1.1), drop = FALSE])

    expect_equal(TST_A[1.9], TST_B[1.9])
    expect_equal(TST_A[1.9, drop = TRUE], TST_B[1.9, drop = TRUE])
    expect_equal(TST_A[1.9, drop = FALSE], TST_B[1.9, drop = FALSE])

    expect_equal(TST_A[c(1.9, 2.9)], TST_B[c(1.9, 2.9)])
    expect_equal(TST_A[c(1.9, 2.9), drop = TRUE], TST_B[c(1.9, 2.9), drop = TRUE])
    expect_equal(TST_A[c(1.9, 2.9), drop = FALSE], TST_B[c(1.9, 2.9), drop = FALSE])

    expect_equal(TST_A[c(2.9, 1.9)], TST_B[c(2.9, 1.9)])
    expect_equal(TST_A[c(2.9, 1.9), drop = TRUE], TST_B[c(2.9, 1.9), drop = TRUE])
    expect_equal(TST_A[c(2.9, 1.9), drop = FALSE], TST_B[c(2.9, 1.9), drop = FALSE])

    expect_equal(TST_A[OUT_OF_BOUNDS_INT], TST_B[OUT_OF_BOUNDS_INT])
    expect_equal(TST_A[OUT_OF_BOUNDS_INT, drop = TRUE], TST_B[OUT_OF_BOUNDS_INT, drop = TRUE])
    expect_equal(TST_A[OUT_OF_BOUNDS_INT, drop = FALSE], TST_B[OUT_OF_BOUNDS_INT, drop = FALSE])

    expect_equal(TST_A[c(OUT_OF_BOUNDS_INT, 2)], TST_B[c(OUT_OF_BOUNDS_INT, 2)])
    expect_equal(TST_A[c(OUT_OF_BOUNDS_INT, 2), drop = TRUE], TST_B[c(OUT_OF_BOUNDS_INT, 2), drop = TRUE])
    expect_equal(TST_A[c(OUT_OF_BOUNDS_INT, 2), drop = FALSE], TST_B[c(OUT_OF_BOUNDS_INT, 2), drop = FALSE])

    expect_equal(TST_A[c(2, OUT_OF_BOUNDS_INT)], TST_B[c(2, OUT_OF_BOUNDS_INT)])
    expect_equal(TST_A[c(2, OUT_OF_BOUNDS_INT), drop = TRUE], TST_B[c(2, OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_equal(TST_A[c(2, OUT_OF_BOUNDS_INT), drop = FALSE], TST_B[c(2, OUT_OF_BOUNDS_INT), drop = FALSE])

    m <- matrix(data = c(1, 1, 2, 2), ncol = 2, byrow = TRUE)
    expect_equal(TST_A[m], TST_B[m])
    expect_equal(TST_A[m, drop = TRUE], TST_B[m, drop = TRUE])
    expect_equal(TST_A[m, drop = FALSE], TST_B[m, drop = FALSE])

    m <- matrix(data = c(1, 1, 2, 2), ncol = 2, byrow = TRUE)
    expect_equal(TST_A[m], TST_B[m])
    expect_equal(TST_A[m, drop = TRUE], TST_B[m, drop = TRUE])
    expect_equal(TST_A[m, drop = FALSE], TST_B[m, drop = FALSE])

    m <- matrix(data = c(2, 2, 1, 1), ncol = 2, byrow = TRUE)
    expect_equal(TST_A[m], TST_B[m])
    expect_equal(TST_A[m, drop = TRUE], TST_B[m, drop = TRUE])
    expect_equal(TST_A[m, drop = FALSE], TST_B[m, drop = FALSE])

    m <- matrix(data = c(1.1, 1.1, 2.1, 2.1), ncol = 2, byrow = TRUE)
    expect_equal(TST_A[m], TST_B[m])
    expect_equal(TST_A[m, drop = TRUE], TST_B[m, drop = TRUE])
    expect_equal(TST_A[m, drop = FALSE], TST_B[m, drop = FALSE])

    m <- matrix(data = c(2.1, 2.1, 1.1, 1.1), ncol = 2, byrow = TRUE)
    expect_equal(TST_A[m], TST_B[m])
    expect_equal(TST_A[m, drop = TRUE], TST_B[m, drop = TRUE])
    expect_equal(TST_A[m, drop = FALSE], TST_B[m, drop = FALSE])

    m <- matrix(data = c(1.9, 1.9, 2.9, 2.9), ncol = 2, byrow = TRUE)
    expect_equal(TST_A[m], TST_B[m])
    expect_equal(TST_A[m, drop = TRUE], TST_B[m, drop = TRUE])
    expect_equal(TST_A[m, drop = FALSE], TST_B[m, drop = FALSE])

    m <- matrix(data = c(2.9, 2.9, 1.9, 1.9), ncol = 2, byrow = TRUE)
    expect_equal(TST_A[m], TST_B[m])
    expect_equal(TST_A[m, drop = TRUE], TST_B[m, drop = TRUE])
    expect_equal(TST_A[m, drop = FALSE], TST_B[m, drop = FALSE])

    m <- matrix(data = c(OUT_OF_BOUNDS_INT, OUT_OF_BOUNDS_INT), ncol = 2, byrow = TRUE)
    expect_error(TST_A[m])
    expect_error(TST_B[m])
    expect_error(TST_A[m, drop = TRUE])
    expect_error(TST_B[m, drop = TRUE])
    expect_error(TST_A[m, drop = FALSE])
    expect_error(TST_B[m, drop = FALSE])

    expect_equal(TST_A[c(1, NA)], TST_B[c(1, NA)])
    expect_equal(TST_A[c(1, NA), drop = TRUE], TST_B[c(1, NA), drop = TRUE])
    expect_equal(TST_A[c(1, NA), drop = FALSE], TST_B[c(1, NA), drop = FALSE])

})

test_that("single indexing by negative integers works", {

    expect_equal(TST_A[-1], TST_B[-1])
    expect_equal(TST_A[-1, drop = TRUE], TST_B[-1, drop = TRUE])
    expect_equal(TST_A[-1, drop = FALSE], TST_B[-1, drop = FALSE])

    expect_equal(TST_A[c(-1, -2)], TST_B[c(-1, -2)])
    expect_equal(TST_A[c(-1, -2), drop = TRUE], TST_B[c(-1, -2), drop = TRUE])
    expect_equal(TST_A[c(-1, -2), drop = FALSE], TST_B[c(-1, -2), drop = FALSE])

    expect_equal(TST_A[c(-2, -1)], TST_B[c(-2, -1)])
    expect_equal(TST_A[c(-2, -1), drop = TRUE], TST_B[c(-2, -1), drop = TRUE])
    expect_equal(TST_A[c(-2, -1), drop = FALSE], TST_B[c(-2, -1), drop = FALSE])

    expect_equal(TST_A[-1.1], TST_B[-1.1])
    expect_equal(TST_A[-1.1, drop = TRUE], TST_B[-1.1, drop = TRUE])
    expect_equal(TST_A[-1.1, drop = FALSE], TST_B[-1.1, drop = FALSE])

    expect_equal(TST_A[c(-1.1, -2.1)], TST_B[c(-1.1, -2.1)])
    expect_equal(TST_A[c(-1.1, -2.1), drop = TRUE], TST_B[c(-1.1, -2.1), drop = TRUE])
    expect_equal(TST_A[c(-1.1, -2.1), drop = FALSE], TST_B[c(-1.1, -2.1), drop = FALSE])

    expect_equal(TST_A[c(-2.1, -1.1)], TST_B[c(-2.1, -1.1)])
    expect_equal(TST_A[c(-2.1, -1.1), drop = TRUE], TST_B[c(-2.1, -1.1), drop = TRUE])
    expect_equal(TST_A[c(-2.1, -1.1), drop = FALSE], TST_B[c(-2.1, -1.1), drop = FALSE])

    expect_equal(TST_A[-1.9], TST_B[-1.9])
    expect_equal(TST_A[-1.9, drop = TRUE], TST_B[-1.9, drop = TRUE])
    expect_equal(TST_A[-1.9, drop = FALSE], TST_B[-1.9, drop = FALSE])

    expect_equal(TST_A[c(-1.9, -2.9)], TST_B[c(-1.9, -2.9)])
    expect_equal(TST_A[c(-1.9, -2.9), drop = TRUE], TST_B[c(-1.9, -2.9), drop = TRUE])
    expect_equal(TST_A[c(-1.9, -2.9), drop = FALSE], TST_B[c(-1.9, -2.9), drop = FALSE])

    expect_equal(TST_A[c(-2.9, -1.9)], TST_B[c(-2.9, -1.9)])
    expect_equal(TST_A[c(-2.9, -1.9), drop = TRUE], TST_B[c(-2.9, -1.9), drop = TRUE])
    expect_equal(TST_A[c(-2.9, -1.9), drop = FALSE], TST_B[c(-2.9, -1.9), drop = FALSE])

    expect_equal(TST_A[-OUT_OF_BOUNDS_INT], TST_B[-OUT_OF_BOUNDS_INT])
    expect_equal(TST_A[-OUT_OF_BOUNDS_INT, drop = TRUE], TST_B[-OUT_OF_BOUNDS_INT, drop = TRUE])
    expect_equal(TST_A[-OUT_OF_BOUNDS_INT, drop = FALSE], TST_B[-OUT_OF_BOUNDS_INT, drop = FALSE])

    expect_equal(TST_A[c(-OUT_OF_BOUNDS_INT, -2)], TST_B[c(-OUT_OF_BOUNDS_INT, -2)])
    expect_equal(TST_A[c(-OUT_OF_BOUNDS_INT, -2), drop = TRUE], TST_B[c(-OUT_OF_BOUNDS_INT, -2), drop = TRUE])
    expect_equal(TST_A[c(-OUT_OF_BOUNDS_INT, -2), drop = FALSE], TST_B[c(-OUT_OF_BOUNDS_INT, -2), drop = FALSE])

    expect_equal(TST_A[c(-2, -OUT_OF_BOUNDS_INT)], TST_B[c(-2, -OUT_OF_BOUNDS_INT)])
    expect_equal(TST_A[c(-2, -OUT_OF_BOUNDS_INT), drop = TRUE], TST_B[c(-2, -OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_equal(TST_A[c(-2, -OUT_OF_BOUNDS_INT), drop = FALSE], TST_B[c(-2, -OUT_OF_BOUNDS_INT), drop = FALSE])

    expect_error(TST_A[c(-1, NA)])
    expect_error(TST_B[c(-1, NA)])
    expect_error(TST_A[c(-1, NA), drop = TRUE])
    expect_error(TST_B[c(-1, NA), drop = TRUE])
    expect_error(TST_A[c(-1, NA), drop = FALSE])
    expect_error(TST_B[c(-1, NA), drop = FALSE])

})

test_that("single indexing by logicals works", {

    expect_equal(TST_A[TRUE], TST_B[TRUE])
    expect_equal(TST_A[TRUE, drop = TRUE], TST_B[TRUE, drop = TRUE])
    expect_equal(TST_A[TRUE, drop = FALSE], TST_B[TRUE, drop = FALSE])

    expect_equal(TST_A[FALSE], TST_B[FALSE])
    expect_equal(TST_A[FALSE, drop = FALSE], TST_B[FALSE, drop = FALSE])
    expect_equal(TST_A[FALSE, drop = FALSE], TST_B[FALSE, drop = FALSE])

    expect_equal(TST_A[c(TRUE, FALSE)], TST_B[c(TRUE, FALSE)])
    expect_equal(TST_A[c(TRUE, FALSE), drop = TRUE], TST_B[c(TRUE, FALSE), drop = TRUE])
    expect_equal(TST_A[c(TRUE, FALSE), drop = FALSE], TST_B[c(TRUE, FALSE), drop = FALSE])

    expect_equal(TST_A[c(FALSE, TRUE)], TST_B[c(FALSE, TRUE)])
    expect_equal(TST_A[c(FALSE, TRUE), drop = TRUE], TST_B[c(FALSE, TRUE), drop = TRUE])
    expect_equal(TST_A[c(FALSE, TRUE), drop = FALSE], TST_B[c(FALSE, TRUE), drop = FALSE])

    expect_equal(TST_A[rep_len(TRUE, OUT_OF_BOUNDS_INT)], TST_B[rep_len(TRUE, OUT_OF_BOUNDS_INT)])
    expect_equal(TST_A[rep_len(TRUE, OUT_OF_BOUNDS_INT), drop = rep_len(TRUE, OUT_OF_BOUNDS_INT)], TST_B[rep_len(TRUE, OUT_OF_BOUNDS_INT), drop = rep_len(TRUE, OUT_OF_BOUNDS_INT)])
    expect_equal(TST_A[rep_len(TRUE, OUT_OF_BOUNDS_INT), drop = FALSE], TST_B[rep_len(TRUE, OUT_OF_BOUNDS_INT), drop = FALSE])

    expect_equal(TST_A[rep_len(FALSE, OUT_OF_BOUNDS_INT)], TST_B[rep_len(FALSE, OUT_OF_BOUNDS_INT)])
    expect_equal(TST_A[rep_len(FALSE, OUT_OF_BOUNDS_INT), drop = rep_len(FALSE, OUT_OF_BOUNDS_INT)], TST_B[rep_len(FALSE, OUT_OF_BOUNDS_INT), drop = rep_len(FALSE, OUT_OF_BOUNDS_INT)])
    expect_equal(TST_A[rep_len(FALSE, OUT_OF_BOUNDS_INT), drop = rep_len(FALSE, OUT_OF_BOUNDS_INT)], TST_B[rep_len(FALSE, OUT_OF_BOUNDS_INT), drop = rep_len(FALSE, OUT_OF_BOUNDS_INT)])

    expect_equal(TST_A[rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT)], TST_B[rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT)])
    expect_equal(TST_A[rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = TRUE], TST_B[rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_equal(TST_A[rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = FALSE], TST_B[rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = FALSE])

    expect_equal(TST_A[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT)], TST_B[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT)])
    expect_equal(TST_A[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = TRUE], TST_B[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_equal(TST_A[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = FALSE], TST_B[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = FALSE])

    m <- matrix(data = rnorm(25), nrow = 5, ncol = 5)
    expect_equal(TST_A[m > 1], TST_B[m > 1])
    expect_equal(TST_A[m > 1, drop = TRUE], TST_B[m > 1, drop = TRUE])
    expect_equal(TST_A[m > 1, drop = FALSE], TST_B[m > 1, drop = FALSE])

    expect_equal(TST_A[c(TRUE, NA)], TST_B[c(TRUE, NA)])
    expect_equal(TST_A[c(TRUE, NA), drop = TRUE], TST_B[c(TRUE, NA), drop = TRUE])
    expect_equal(TST_A[c(TRUE, NA), drop = FALSE], TST_B[c(TRUE, NA), drop = FALSE])

    expect_equal(TST_A[c(FALSE, NA)], TST_B[c(FALSE, NA)])
    expect_equal(TST_A[c(FALSE, NA), drop = TRUE], TST_B[c(FALSE, NA), drop = TRUE])
    expect_equal(TST_A[c(FALSE, NA), drop = FALSE], TST_B[c(FALSE, NA), drop = FALSE])

})

test_that("single indexing by characters works", {

    if (is.null(dimnames(TST_A))) {
        skip("skipping character indexing because dimnames are NULL")
    }

    NAME_1 <- rownames(TST_A)[1]
    NAME_2 <- rownames(TST_A)[2]

    expect_equal(TST_A[NAME_1], TST_B[NAME_1])
    expect_equal(TST_A[NAME_1, drop = TRUE], TST_B[NAME_1, drop = TRUE])
    expect_equal(TST_A[NAME_1, drop = FALSE], TST_B[NAME_1, drop = FALSE])

    expect_equal(TST_A[OUT_OF_BOUNDS_CHAR], TST_B[OUT_OF_BOUNDS_CHAR])
    expect_equal(TST_A[OUT_OF_BOUNDS_CHAR, drop = TRUE], TST_B[OUT_OF_BOUNDS_CHAR, drop = TRUE])
    expect_equal(TST_A[OUT_OF_BOUNDS_CHAR, drop = FALSE], TST_B[OUT_OF_BOUNDS_CHAR, drop = FALSE])

    m <- matrix(data = c(NAME_1, NAME_1, NAME_2, NAME_2), ncol = 2, byrow = TRUE)
    expect_equal(TST_A[m], TST_B[m])
    expect_equal(TST_A[m, drop = TRUE], TST_B[m, drop = TRUE])
    expect_equal(TST_A[m, drop = FALSE], TST_B[m, drop = FALSE])

    m <- matrix(data = c(NAME_2, NAME_2, NAME_1, NAME_1), ncol = 2, byrow = TRUE)
    expect_equal(TST_A[m], TST_B[m])
    expect_equal(TST_A[m, drop = TRUE], TST_B[m, drop = TRUE])
    expect_equal(TST_A[m, drop = FALSE], TST_B[m, drop = FALSE])

    m <- matrix(data = c(OUT_OF_BOUNDS_CHAR, OUT_OF_BOUNDS_CHAR), ncol = 2, byrow = TRUE)
    expect_error(TST_A[m])
    expect_error(TST_B[m])
    expect_error(TST_A[m, drop = TRUE])
    expect_error(TST_B[m, drop = TRUE])
    expect_error(TST_A[m, drop = FALSE])
    expect_error(TST_B[m, drop = FALSE])

    expect_equal(TST_A[c(NAME_1, NA)], TST_B[c(NAME_1, NA)])
    expect_equal(TST_A[c(NAME_1, NA), drop = TRUE], TST_B[c(NAME_1, NA), drop = TRUE])
    expect_equal(TST_A[c(NAME_1, NA), drop = FALSE], TST_B[c(NAME_1, NA), drop = FALSE])

})

test_that("single indexing by NA works", {

    expect_equal(TST_A[NA], TST_B[NA])
    expect_equal(TST_A[NA, drop = TRUE], TST_B[NA, drop = TRUE])
    expect_equal(TST_A[NA, drop = FALSE], TST_B[NA, drop = FALSE])

    expect_equal(TST_A[NA_integer_], TST_B[NA_integer_])
    expect_equal(TST_A[NA_integer_, drop = TRUE], TST_B[NA_integer_, drop = TRUE])
    expect_equal(TST_A[NA_integer_, drop = FALSE], TST_B[NA_integer_, drop = FALSE])

    expect_equal(TST_A[NA_character_], TST_B[NA_character_])
    expect_equal(TST_A[NA_character_, drop = TRUE], TST_B[NA_character_, drop = TRUE])
    expect_equal(TST_A[NA_character_, drop = FALSE], TST_B[NA_character_, drop = FALSE])

})

test_that("single indexing by zero works", {

    expect_error(TST_A[0]) # Not implemented
    expect_error(TST_A[0, drop = TRUE]) # Not implemented
    expect_error(TST_A[0, drop = FALSE]) # Not implemented

    expect_equal(TST_A[c(0, 1)], TST_B[c(0, 1)])
    expect_equal(TST_A[c(0, 1), drop = TRUE], TST_B[c(0, 1), drop = TRUE])
    expect_equal(TST_A[c(0, 1), drop = FALSE], TST_B[c(0, 1), drop = FALSE])

    expect_equal(TST_A[c(0, -1)], TST_B[c(0, -1)])
    expect_equal(TST_A[c(0, -1), drop = TRUE], TST_B[c(0, -1), drop = TRUE])
    expect_equal(TST_A[c(0, -1), drop = FALSE], TST_B[c(0, -1), drop = FALSE])

    expect_error(TST_A[c(0, 1, -1)])
    expect_error(TST_B[c(0, 1, -1)])
    expect_error(TST_A[c(0, 1, -1), drop = TRUE])
    expect_error(TST_B[c(0, 1, -1), drop = TRUE])
    expect_error(TST_A[c(0, 1, -1), drop = FALSE])
    expect_error(TST_B[c(0, 1, -1), drop = FALSE])

})

test_that("multi indexing by nothing works", {

    expect_equal(TST_A[, ], TST_B[, ])
    expect_equal(TST_A[, , drop = TRUE], TST_B[, , drop = TRUE])
    expect_equal(TST_A[, , drop = FALSE], TST_B[, , drop = FALSE])

})

test_that("multi indexing by positive integers works", {

    expect_equal(TST_A[1, ], TST_B[1, ])
    expect_equal(TST_A[1, , drop = TRUE], TST_B[1, , drop = TRUE])
    expect_equal(TST_A[1, , drop = FALSE], TST_B[1, , drop = FALSE])

    expect_equal(TST_A[, 1], TST_B[, 1])
    expect_equal(TST_A[, 1, drop = TRUE], TST_B[, 1, drop = TRUE])
    expect_equal(TST_A[, 1, drop = FALSE], TST_B[, 1, drop = FALSE])

    expect_equal(TST_A[1, 1], TST_B[1, 1])
    expect_equal(TST_A[1, 1, drop = TRUE], TST_B[1, 1, drop = TRUE])
    expect_equal(TST_A[1, 1, drop = FALSE], TST_B[1, 1, drop = FALSE])

    expect_equal(TST_A[c(1, 2), ], TST_B[c(1, 2), ])
    expect_equal(TST_A[c(1, 2), , drop = TRUE], TST_B[c(1, 2), , drop = TRUE])
    expect_equal(TST_A[c(1, 2), , drop = FALSE], TST_B[c(1, 2), , drop = FALSE])

    expect_equal(TST_A[, c(1, 2)], TST_B[, c(1, 2)])
    expect_equal(TST_A[, c(1, 2), drop = TRUE], TST_B[, c(1, 2), drop = TRUE])
    expect_equal(TST_A[, c(1, 2), drop = FALSE], TST_B[, c(1, 2), drop = FALSE])

    expect_equal(TST_A[c(1, 2), c(1, 2)], TST_B[c(1, 2), c(1, 2)])
    expect_equal(TST_A[c(1, 2), c(1, 2), drop = TRUE], TST_B[c(1, 2), c(1, 2), drop = TRUE])
    expect_equal(TST_A[c(1, 2), c(1, 2), drop = FALSE], TST_B[c(1, 2), c(1, 2), drop = FALSE])

    expect_equal(TST_A[c(2, 1), ], TST_B[c(2, 1), ])
    expect_equal(TST_A[c(2, 1), , drop = TRUE], TST_B[c(2, 1), , drop = TRUE])
    expect_equal(TST_A[c(2, 1), , drop = FALSE], TST_B[c(2, 1), , drop = FALSE])

    expect_equal(TST_A[, c(2, 1)], TST_B[, c(2, 1)])
    expect_equal(TST_A[, c(2, 1), drop = TRUE], TST_B[, c(2, 1), drop = TRUE])
    expect_equal(TST_A[, c(2, 1), drop = FALSE], TST_B[, c(2, 1), drop = FALSE])

    expect_equal(TST_A[c(2, 1), c(2, 1)], TST_B[c(2, 1), c(2, 1)])
    expect_equal(TST_A[c(2, 1), c(2, 1), drop = TRUE], TST_B[c(2, 1), c(2, 1), drop = TRUE])
    expect_equal(TST_A[c(2, 1), c(2, 1), drop = FALSE], TST_B[c(2, 1), c(2, 1), drop = FALSE])

    expect_equal(TST_A[1.1, ], TST_B[1.1, ])
    expect_equal(TST_A[1.1, , drop = TRUE], TST_B[1.1, , drop = TRUE])
    expect_equal(TST_A[1.1, , drop = FALSE], TST_B[1.1, , drop = FALSE])

    expect_equal(TST_A[, 1.1], TST_B[, 1.1])
    expect_equal(TST_A[, 1.1, drop = TRUE], TST_B[, 1.1, drop = TRUE])
    expect_equal(TST_A[, 1.1, drop = FALSE], TST_B[, 1.1, drop = FALSE])

    expect_equal(TST_A[1.1, 1.1], TST_B[1.1, 1.1])
    expect_equal(TST_A[1.1, 1.1, drop = TRUE], TST_B[1.1, 1.1, drop = TRUE])
    expect_equal(TST_A[1.1, 1.1, drop = FALSE], TST_B[1.1, 1.1, drop = FALSE])

    expect_equal(TST_A[c(1.1, 2.1), ], TST_B[c(1.1, 2.1), ])
    expect_equal(TST_A[c(1.1, 2.1), , drop = TRUE], TST_B[c(1.1, 2.1), , drop = TRUE])
    expect_equal(TST_A[c(1.1, 2.1), , drop = FALSE], TST_B[c(1.1, 2.1), , drop = FALSE])

    expect_equal(TST_A[, c(1.1, 2.1)], TST_B[, c(1.1, 2.1)])
    expect_equal(TST_A[, c(1.1, 2.1), drop = TRUE], TST_B[, c(1.1, 2.1), drop = TRUE])
    expect_equal(TST_A[, c(1.1, 2.1), drop = FALSE], TST_B[, c(1.1, 2.1), drop = FALSE])

    expect_equal(TST_A[c(1.1, 2.1), c(1.1, 2.1)], TST_B[c(1.1, 2.1), c(1.1, 2.1)])
    expect_equal(TST_A[c(1.1, 2.1), c(1.1, 2.1), drop = TRUE], TST_B[c(1.1, 2.1), c(1.1, 2.1), drop = TRUE])
    expect_equal(TST_A[c(1.1, 2.1), c(1.1, 2.1), drop = FALSE], TST_B[c(1.1, 2.1), c(1.1, 2.1), drop = FALSE])

    expect_equal(TST_A[c(2.1, 1.1), ], TST_B[c(2.1, 1.1), ])
    expect_equal(TST_A[c(2.1, 1.1), , drop = TRUE], TST_B[c(2.1, 1.1), , drop = TRUE])
    expect_equal(TST_A[c(2.1, 1.1), , drop = FALSE], TST_B[c(2.1, 1.1), , drop = FALSE])

    expect_equal(TST_A[, c(2.1, 1.1)], TST_B[, c(2.1, 1.1)])
    expect_equal(TST_A[, c(2.1, 1.1), drop = TRUE], TST_B[, c(2.1, 1.1), drop = TRUE])
    expect_equal(TST_A[, c(2.1, 1.1), drop = FALSE], TST_B[, c(2.1, 1.1), drop = FALSE])

    expect_equal(TST_A[c(2.1, 1.1), c(2.1, 1.1)], TST_B[c(2.1, 1.1), c(2.1, 1.1)])
    expect_equal(TST_A[c(2.1, 1.1), c(2.1, 1.1), drop = TRUE], TST_B[c(2.1, 1.1), c(2.1, 1.1), drop = TRUE])
    expect_equal(TST_A[c(2.1, 1.1), c(2.1, 1.1), drop = FALSE], TST_B[c(2.1, 1.1), c(2.1, 1.1), drop = FALSE])

    expect_equal(TST_A[1.9, ], TST_B[1.9, ])
    expect_equal(TST_A[1.9, , drop = TRUE], TST_B[1.9, , drop = TRUE])
    expect_equal(TST_A[1.9, , drop = FALSE], TST_B[1.9, , drop = FALSE])

    expect_equal(TST_A[, 1.9], TST_B[, 1.9])
    expect_equal(TST_A[, 1.9, drop = TRUE], TST_B[, 1.9, drop = TRUE])
    expect_equal(TST_A[, 1.9, drop = FALSE], TST_B[, 1.9, drop = FALSE])

    expect_equal(TST_A[1.9, 1.9], TST_B[1.9, 1.9])
    expect_equal(TST_A[1.9, 1.9, drop = TRUE], TST_B[1.9, 1.9, drop = TRUE])
    expect_equal(TST_A[1.9, 1.9, drop = FALSE], TST_B[1.9, 1.9, drop = FALSE])

    expect_equal(TST_A[c(1.9, 2.9), ], TST_B[c(1.9, 2.9), ])
    expect_equal(TST_A[c(1.9, 2.9), , drop = TRUE], TST_B[c(1.9, 2.9), , drop = TRUE])
    expect_equal(TST_A[c(1.9, 2.9), , drop = FALSE], TST_B[c(1.9, 2.9), , drop = FALSE])

    expect_equal(TST_A[, c(1.9, 2.9)], TST_B[, c(1.9, 2.9)])
    expect_equal(TST_A[, c(1.9, 2.9), drop = TRUE], TST_B[, c(1.9, 2.9), drop = TRUE])
    expect_equal(TST_A[, c(1.9, 2.9), drop = FALSE], TST_B[, c(1.9, 2.9), drop = FALSE])

    expect_equal(TST_A[c(1.9, 2.9), c(1.9, 2.9)], TST_B[c(1.9, 2.9), c(1.9, 2.9)])
    expect_equal(TST_A[c(1.9, 2.9), c(1.9, 2.9), drop = TRUE], TST_B[c(1.9, 2.9), c(1.9, 2.9), drop = TRUE])
    expect_equal(TST_A[c(1.9, 2.9), c(1.9, 2.9), drop = FALSE], TST_B[c(1.9, 2.9), c(1.9, 2.9), drop = FALSE])

    expect_equal(TST_A[c(2.9, 1.9), ], TST_B[c(2.9, 1.9), ])
    expect_equal(TST_A[c(2.9, 1.9), , drop = TRUE], TST_B[c(2.9, 1.9), , drop = TRUE])
    expect_equal(TST_A[c(2.9, 1.9), , drop = FALSE], TST_B[c(2.9, 1.9), , drop = FALSE])

    expect_equal(TST_A[, c(2.9, 1.9)], TST_B[, c(2.9, 1.9)])
    expect_equal(TST_A[, c(2.9, 1.9), drop = TRUE], TST_B[, c(2.9, 1.9), drop = TRUE])
    expect_equal(TST_A[, c(2.9, 1.9), drop = FALSE], TST_B[, c(2.9, 1.9), drop = FALSE])

    expect_equal(TST_A[c(2.9, 1.9), c(2.9, 1.9)], TST_B[c(2.9, 1.9), c(2.9, 1.9)])
    expect_equal(TST_A[c(2.9, 1.9), c(2.9, 1.9), drop = TRUE], TST_B[c(2.9, 1.9), c(2.9, 1.9), drop = TRUE])
    expect_equal(TST_A[c(2.9, 1.9), c(2.9, 1.9), drop = FALSE], TST_B[c(2.9, 1.9), c(2.9, 1.9), drop = FALSE])

    expect_error(TST_A[OUT_OF_BOUNDS_INT, ])
    expect_error(TST_B[OUT_OF_BOUNDS_INT, ])
    expect_error(TST_A[OUT_OF_BOUNDS_INT, , drop = TRUE])
    expect_error(TST_B[OUT_OF_BOUNDS_INT, , drop = TRUE])
    expect_error(TST_A[OUT_OF_BOUNDS_INT, , drop = FALSE])
    expect_error(TST_B[OUT_OF_BOUNDS_INT, , drop = FALSE])

    expect_error(TST_A[, OUT_OF_BOUNDS_INT])
    expect_error(TST_B[, OUT_OF_BOUNDS_INT])
    expect_error(TST_A[, OUT_OF_BOUNDS_INT, drop = TRUE])
    expect_error(TST_B[, OUT_OF_BOUNDS_INT, drop = TRUE])
    expect_error(TST_A[, OUT_OF_BOUNDS_INT, drop = FALSE])
    expect_error(TST_B[, OUT_OF_BOUNDS_INT, drop = FALSE])

    expect_error(TST_A[OUT_OF_BOUNDS_INT, OUT_OF_BOUNDS_INT])
    expect_error(TST_B[OUT_OF_BOUNDS_INT, OUT_OF_BOUNDS_INT])
    expect_error(TST_A[OUT_OF_BOUNDS_INT, OUT_OF_BOUNDS_INT, drop = TRUE])
    expect_error(TST_B[OUT_OF_BOUNDS_INT, OUT_OF_BOUNDS_INT, drop = TRUE])
    expect_error(TST_A[OUT_OF_BOUNDS_INT, OUT_OF_BOUNDS_INT, drop = FALSE])
    expect_error(TST_B[OUT_OF_BOUNDS_INT, OUT_OF_BOUNDS_INT, drop = FALSE])

    expect_error(TST_A[c(OUT_OF_BOUNDS_INT, 2), ])
    expect_error(TST_B[c(OUT_OF_BOUNDS_INT, 2), ])
    expect_error(TST_A[c(OUT_OF_BOUNDS_INT, 2), , drop = TRUE])
    expect_error(TST_B[c(OUT_OF_BOUNDS_INT, 2), , drop = TRUE])
    expect_error(TST_A[c(OUT_OF_BOUNDS_INT, 2), , drop = FALSE])
    expect_error(TST_B[c(OUT_OF_BOUNDS_INT, 2), , drop = FALSE])

    expect_error(TST_A[, c(OUT_OF_BOUNDS_INT, 2)])
    expect_error(TST_B[, c(OUT_OF_BOUNDS_INT, 2)])
    expect_error(TST_A[, c(OUT_OF_BOUNDS_INT, 2), drop = TRUE])
    expect_error(TST_B[, c(OUT_OF_BOUNDS_INT, 2), drop = TRUE])
    expect_error(TST_A[, c(OUT_OF_BOUNDS_INT, 2), drop = FALSE])
    expect_error(TST_B[, c(OUT_OF_BOUNDS_INT, 2), drop = FALSE])

    expect_error(TST_A[c(OUT_OF_BOUNDS_INT, 2), c(OUT_OF_BOUNDS_INT, 2)])
    expect_error(TST_B[c(OUT_OF_BOUNDS_INT, 2), c(OUT_OF_BOUNDS_INT, 2)])
    expect_error(TST_A[c(OUT_OF_BOUNDS_INT, 2), c(OUT_OF_BOUNDS_INT, 2), drop = TRUE])
    expect_error(TST_B[c(OUT_OF_BOUNDS_INT, 2), c(OUT_OF_BOUNDS_INT, 2), drop = TRUE])
    expect_error(TST_A[c(OUT_OF_BOUNDS_INT, 2), c(OUT_OF_BOUNDS_INT, 2), drop = FALSE])
    expect_error(TST_B[c(OUT_OF_BOUNDS_INT, 2), c(OUT_OF_BOUNDS_INT, 2), drop = FALSE])

    expect_error(TST_A[c(2, OUT_OF_BOUNDS_INT), ])
    expect_error(TST_B[c(2, OUT_OF_BOUNDS_INT), ])
    expect_error(TST_A[c(2, OUT_OF_BOUNDS_INT), , drop = TRUE])
    expect_error(TST_B[c(2, OUT_OF_BOUNDS_INT), , drop = TRUE])
    expect_error(TST_A[c(2, OUT_OF_BOUNDS_INT), , drop = FALSE])
    expect_error(TST_B[c(2, OUT_OF_BOUNDS_INT), , drop = FALSE])

    expect_error(TST_A[, c(2, OUT_OF_BOUNDS_INT)])
    expect_error(TST_B[, c(2, OUT_OF_BOUNDS_INT)])
    expect_error(TST_A[, c(2, OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_error(TST_B[, c(2, OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_error(TST_A[, c(2, OUT_OF_BOUNDS_INT), drop = FALSE])
    expect_error(TST_B[, c(2, OUT_OF_BOUNDS_INT), drop = FALSE])

    expect_error(TST_A[c(2, OUT_OF_BOUNDS_INT), c(2, OUT_OF_BOUNDS_INT)])
    expect_error(TST_B[c(2, OUT_OF_BOUNDS_INT), c(2, OUT_OF_BOUNDS_INT)])
    expect_error(TST_A[c(2, OUT_OF_BOUNDS_INT), c(2, OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_error(TST_B[c(2, OUT_OF_BOUNDS_INT), c(2, OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_error(TST_A[c(2, OUT_OF_BOUNDS_INT), c(2, OUT_OF_BOUNDS_INT), drop = FALSE])
    expect_error(TST_B[c(2, OUT_OF_BOUNDS_INT), c(2, OUT_OF_BOUNDS_INT), drop = FALSE])

    expect_equal(TST_A[c(1, NA), ], TST_B[c(1, NA), ])
    expect_equal(TST_A[c(1, NA), , drop = TRUE], TST_B[c(1, NA), , drop = TRUE])
    expect_equal(TST_A[c(1, NA), , drop = FALSE], TST_B[c(1, NA), , drop = FALSE])

    expect_equal(TST_A[, c(1, NA)], TST_B[, c(1, NA)])
    expect_equal(TST_A[, c(1, NA), drop = TRUE], TST_B[, c(1, NA), drop = TRUE])
    expect_equal(TST_A[, c(1, NA), drop = FALSE], TST_B[, c(1, NA), drop = FALSE])

    expect_equal(TST_A[c(1, NA), c(1, NA)], TST_B[c(1, NA), c(1, NA)])
    expect_equal(TST_A[c(1, NA), c(1, NA), drop = TRUE], TST_B[c(1, NA), c(1, NA), drop = TRUE])
    expect_equal(TST_A[c(1, NA), c(1, NA), drop = FALSE], TST_B[c(1, NA), c(1, NA), drop = FALSE])

})

test_that("multi indexing by negative integers works", {

    expect_equal(TST_A[c(-1), ], TST_B[c(-1), ])
    expect_equal(TST_A[c(-1), , drop = TRUE], TST_B[c(-1), , drop = TRUE])
    expect_equal(TST_A[c(-1), , drop = FALSE], TST_B[c(-1), , drop = FALSE])

    expect_equal(TST_A[, c(-1)], TST_B[, c(-1)])
    expect_equal(TST_A[, c(-1), drop = TRUE], TST_B[, c(-1), drop = TRUE])
    expect_equal(TST_A[, c(-1), drop = FALSE], TST_B[, c(-1), drop = FALSE])

    expect_equal(TST_A[c(-1), c(-1)], TST_B[c(-1), c(-1)])
    expect_equal(TST_A[c(-1), c(-1), drop = TRUE], TST_B[c(-1), c(-1), drop = TRUE])
    expect_equal(TST_A[c(-1), c(-1), drop = FALSE], TST_B[c(-1), c(-1), drop = FALSE])

    expect_equal(TST_A[c(-1, -2), ], TST_B[c(-1, -2), ])
    expect_equal(TST_A[c(-1, -2), , drop = TRUE], TST_B[c(-1, -2), , drop = TRUE])
    expect_equal(TST_A[c(-1, -2), , drop = FALSE], TST_B[c(-1, -2), , drop = FALSE])

    expect_equal(TST_A[, c(-1, -2)], TST_B[, c(-1, -2)])
    expect_equal(TST_A[, c(-1, -2), drop = TRUE], TST_B[, c(-1, -2), drop = TRUE])
    expect_equal(TST_A[, c(-1, -2), drop = FALSE], TST_B[, c(-1, -2), drop = FALSE])

    expect_equal(TST_A[c(-1, -2), c(-1, -2)], TST_B[c(-1, -2), c(-1, -2)])
    expect_equal(TST_A[c(-1, -2), c(-1, -2), drop = TRUE], TST_B[c(-1, -2), c(-1, -2), drop = TRUE])
    expect_equal(TST_A[c(-1, -2), c(-1, -2), drop = FALSE], TST_B[c(-1, -2), c(-1, -2), drop = FALSE])

    expect_equal(TST_A[c(-2, -1), ], TST_B[c(-2, -1), ])
    expect_equal(TST_A[c(-2, -1), , drop = TRUE], TST_B[c(-2, -1), , drop = TRUE])
    expect_equal(TST_A[c(-2, -1), , drop = FALSE], TST_B[c(-2, -1), , drop = FALSE])

    expect_equal(TST_A[, c(-2, -1)], TST_B[, c(-2, -1)])
    expect_equal(TST_A[, c(-2, -1), drop = TRUE], TST_B[, c(-2, -1), drop = TRUE])
    expect_equal(TST_A[, c(-2, -1), drop = FALSE], TST_B[, c(-2, -1), drop = FALSE])

    expect_equal(TST_A[c(-2, -1), c(-2, -1)], TST_B[c(-2, -1), c(-2, -1)])
    expect_equal(TST_A[c(-2, -1), c(-2, -1), drop = TRUE], TST_B[c(-2, -1), c(-2, -1), drop = TRUE])
    expect_equal(TST_A[c(-2, -1), c(-2, -1), drop = FALSE], TST_B[c(-2, -1), c(-2, -1), drop = FALSE])

    expect_equal(TST_A[c(-1.1), ], TST_B[c(-1.1), ])
    expect_equal(TST_A[c(-1.1), , drop = TRUE], TST_B[c(-1.1), , drop = TRUE])
    expect_equal(TST_A[c(-1.1), , drop = FALSE], TST_B[c(-1.1), , drop = FALSE])

    expect_equal(TST_A[, c(-1.1)], TST_B[, c(-1.1)])
    expect_equal(TST_A[, c(-1.1), drop = TRUE], TST_B[, c(-1.1), drop = TRUE])
    expect_equal(TST_A[, c(-1.1), drop = FALSE], TST_B[, c(-1.1), drop = FALSE])

    expect_equal(TST_A[c(-1.1), c(-1.1)], TST_B[c(-1.1), c(-1.1)])
    expect_equal(TST_A[c(-1.1), c(-1.1), drop = TRUE], TST_B[c(-1.1), c(-1.1), drop = TRUE])
    expect_equal(TST_A[c(-1.1), c(-1.1), drop = FALSE], TST_B[c(-1.1), c(-1.1), drop = FALSE])

    expect_equal(TST_A[c(-1.1, -2.1), ], TST_B[c(-1.1, -2.1), ])
    expect_equal(TST_A[c(-1.1, -2.1), , drop = TRUE], TST_B[c(-1.1, -2.1), , drop = TRUE])
    expect_equal(TST_A[c(-1.1, -2.1), , drop = FALSE], TST_B[c(-1.1, -2.1), , drop = FALSE])

    expect_equal(TST_A[, c(-1.1, -2.1)], TST_B[, c(-1.1, -2.1)])
    expect_equal(TST_A[, c(-1.1, -2.1), drop = TRUE], TST_B[, c(-1.1, -2.1), drop = TRUE])
    expect_equal(TST_A[, c(-1.1, -2.1), drop = FALSE], TST_B[, c(-1.1, -2.1), drop = FALSE])

    expect_equal(TST_A[c(-1.1, -2.1), c(-1.1, -2.1)], TST_B[c(-1.1, -2.1), c(-1.1, -2.1)])
    expect_equal(TST_A[c(-1.1, -2.1), c(-1.1, -2.1), drop = TRUE], TST_B[c(-1.1, -2.1), c(-1.1, -2.1), drop = TRUE])
    expect_equal(TST_A[c(-1.1, -2.1), c(-1.1, -2.1), drop = FALSE], TST_B[c(-1.1, -2.1), c(-1.1, -2.1), drop = FALSE])

    expect_equal(TST_A[c(-2.1, -1.1), ], TST_B[c(-2.1, -1.1), ])
    expect_equal(TST_A[c(-2.1, -1.1), , drop = TRUE], TST_B[c(-2.1, -1.1), , drop = TRUE])
    expect_equal(TST_A[c(-2.1, -1.1), , drop = FALSE], TST_B[c(-2.1, -1.1), , drop = FALSE])

    expect_equal(TST_A[, c(-2.1, -1.1)], TST_B[, c(-2.1, -1.1)])
    expect_equal(TST_A[, c(-2.1, -1.1), drop = TRUE], TST_B[, c(-2.1, -1.1), drop = TRUE])
    expect_equal(TST_A[, c(-2.1, -1.1), drop = FALSE], TST_B[, c(-2.1, -1.1), drop = FALSE])

    expect_equal(TST_A[c(-2.1, -1.1), c(-2.1, -1.1)], TST_B[c(-2.1, -1.1), c(-2.1, -1.1)])
    expect_equal(TST_A[c(-2.1, -1.1), c(-2.1, -1.1), drop = TRUE], TST_B[c(-2.1, -1.1), c(-2.1, -1.1), drop = TRUE])
    expect_equal(TST_A[c(-2.1, -1.1), c(-2.1, -1.1), drop = FALSE], TST_B[c(-2.1, -1.1), c(-2.1, -1.1), drop = FALSE])

    expect_equal(TST_A[c(-1.9), ], TST_B[c(-1.9), ])
    expect_equal(TST_A[c(-1.9), , drop = TRUE], TST_B[c(-1.9), , drop = TRUE])
    expect_equal(TST_A[c(-1.9), , drop = FALSE], TST_B[c(-1.9), , drop = FALSE])

    expect_equal(TST_A[, c(-1.9)], TST_B[, c(-1.9)])
    expect_equal(TST_A[, c(-1.9), drop = TRUE], TST_B[, c(-1.9), drop = TRUE])
    expect_equal(TST_A[, c(-1.9), drop = FALSE], TST_B[, c(-1.9), drop = FALSE])

    expect_equal(TST_A[c(-1.9), c(-1.9)], TST_B[c(-1.9), c(-1.9)])
    expect_equal(TST_A[c(-1.9), c(-1.9), drop = TRUE], TST_B[c(-1.9), c(-1.9), drop = TRUE])
    expect_equal(TST_A[c(-1.9), c(-1.9), drop = FALSE], TST_B[c(-1.9), c(-1.9), drop = FALSE])

    expect_equal(TST_A[c(-1.9, -2.9), ], TST_B[c(-1.9, -2.9), ])
    expect_equal(TST_A[c(-1.9, -2.9), , drop = TRUE], TST_B[c(-1.9, -2.9), , drop = TRUE])
    expect_equal(TST_A[c(-1.9, -2.9), , drop = FALSE], TST_B[c(-1.9, -2.9), , drop = FALSE])

    expect_equal(TST_A[, c(-1.9, -2.9)], TST_B[, c(-1.9, -2.9)])
    expect_equal(TST_A[, c(-1.9, -2.9), drop = TRUE], TST_B[, c(-1.9, -2.9), drop = TRUE])
    expect_equal(TST_A[, c(-1.9, -2.9), drop = FALSE], TST_B[, c(-1.9, -2.9), drop = FALSE])

    expect_equal(TST_A[c(-1.9, -2.9), c(-1.9, -2.9)], TST_B[c(-1.9, -2.9), c(-1.9, -2.9)])
    expect_equal(TST_A[c(-1.9, -2.9), c(-1.9, -2.9), drop = TRUE], TST_B[c(-1.9, -2.9), c(-1.9, -2.9), drop = TRUE])
    expect_equal(TST_A[c(-1.9, -2.9), c(-1.9, -2.9), drop = FALSE], TST_B[c(-1.9, -2.9), c(-1.9, -2.9), drop = FALSE])

    expect_equal(TST_A[c(-2.9, -1.9), ], TST_B[c(-2.9, -1.9), ])
    expect_equal(TST_A[c(-2.9, -1.9), , drop = TRUE], TST_B[c(-2.9, -1.9), , drop = TRUE])
    expect_equal(TST_A[c(-2.9, -1.9), , drop = FALSE], TST_B[c(-2.9, -1.9), , drop = FALSE])

    expect_equal(TST_A[, c(-2.9, -1.9)], TST_B[, c(-2.9, -1.9)])
    expect_equal(TST_A[, c(-2.9, -1.9), drop = TRUE], TST_B[, c(-2.9, -1.9), drop = TRUE])
    expect_equal(TST_A[, c(-2.9, -1.9), drop = FALSE], TST_B[, c(-2.9, -1.9), drop = FALSE])

    expect_equal(TST_A[c(-2.9, -1.9), c(-2.9, -1.9)], TST_B[c(-2.9, -1.9), c(-2.9, -1.9)])
    expect_equal(TST_A[c(-2.9, -1.9), c(-2.9, -1.9), drop = TRUE], TST_B[c(-2.9, -1.9), c(-2.9, -1.9), drop = TRUE])
    expect_equal(TST_A[c(-2.9, -1.9), c(-2.9, -1.9), drop = FALSE], TST_B[c(-2.9, -1.9), c(-2.9, -1.9), drop = FALSE])

    expect_equal(TST_A[c(-OUT_OF_BOUNDS_INT), ], TST_B[c(-OUT_OF_BOUNDS_INT), ])
    expect_equal(TST_A[c(-OUT_OF_BOUNDS_INT), , drop = TRUE], TST_B[c(-OUT_OF_BOUNDS_INT), , drop = TRUE])
    expect_equal(TST_A[c(-OUT_OF_BOUNDS_INT), , drop = FALSE], TST_B[c(-OUT_OF_BOUNDS_INT), , drop = FALSE])

    expect_equal(TST_A[, c(-OUT_OF_BOUNDS_INT)], TST_B[, c(-OUT_OF_BOUNDS_INT)])
    expect_equal(TST_A[, c(-OUT_OF_BOUNDS_INT), drop = TRUE], TST_B[, c(-OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_equal(TST_A[, c(-OUT_OF_BOUNDS_INT), drop = FALSE], TST_B[, c(-OUT_OF_BOUNDS_INT), drop = FALSE])

    expect_equal(TST_A[c(-OUT_OF_BOUNDS_INT), c(-OUT_OF_BOUNDS_INT)], TST_B[c(-OUT_OF_BOUNDS_INT), c(-OUT_OF_BOUNDS_INT)])
    expect_equal(TST_A[c(-OUT_OF_BOUNDS_INT), c(-OUT_OF_BOUNDS_INT), drop = TRUE], TST_B[c(-OUT_OF_BOUNDS_INT), c(-OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_equal(TST_A[c(-OUT_OF_BOUNDS_INT), c(-OUT_OF_BOUNDS_INT), drop = FALSE], TST_B[c(-OUT_OF_BOUNDS_INT), c(-OUT_OF_BOUNDS_INT), drop = FALSE])

    expect_equal(TST_A[c(-OUT_OF_BOUNDS_INT, -2), ], TST_B[c(-OUT_OF_BOUNDS_INT, -2), ])
    expect_equal(TST_A[c(-OUT_OF_BOUNDS_INT, -2), , drop = TRUE], TST_B[c(-OUT_OF_BOUNDS_INT, -2), , drop = TRUE])
    expect_equal(TST_A[c(-OUT_OF_BOUNDS_INT, -2), , drop = FALSE], TST_B[c(-OUT_OF_BOUNDS_INT, -2), , drop = FALSE])

    expect_equal(TST_A[, c(-OUT_OF_BOUNDS_INT, -2)], TST_B[, c(-OUT_OF_BOUNDS_INT, -2)])
    expect_equal(TST_A[, c(-OUT_OF_BOUNDS_INT, -2), drop = TRUE], TST_B[, c(-OUT_OF_BOUNDS_INT, -2), drop = TRUE])
    expect_equal(TST_A[, c(-OUT_OF_BOUNDS_INT, -2), drop = FALSE], TST_B[, c(-OUT_OF_BOUNDS_INT, -2), drop = FALSE])

    expect_equal(TST_A[c(-OUT_OF_BOUNDS_INT, -2), c(-OUT_OF_BOUNDS_INT, -2)], TST_B[c(-OUT_OF_BOUNDS_INT, -2), c(-OUT_OF_BOUNDS_INT, -2)])
    expect_equal(TST_A[c(-OUT_OF_BOUNDS_INT, -2), c(-OUT_OF_BOUNDS_INT, -2), drop = TRUE], TST_B[c(-OUT_OF_BOUNDS_INT, -2), c(-OUT_OF_BOUNDS_INT, -2), drop = TRUE])
    expect_equal(TST_A[c(-OUT_OF_BOUNDS_INT, -2), c(-OUT_OF_BOUNDS_INT, -2), drop = FALSE], TST_B[c(-OUT_OF_BOUNDS_INT, -2), c(-OUT_OF_BOUNDS_INT, -2), drop = FALSE])

    expect_equal(TST_A[c(-2, -OUT_OF_BOUNDS_INT), ], TST_B[c(-2, -OUT_OF_BOUNDS_INT), ])
    expect_equal(TST_A[c(-2, -OUT_OF_BOUNDS_INT), , drop = TRUE], TST_B[c(-2, -OUT_OF_BOUNDS_INT), , drop = TRUE])
    expect_equal(TST_A[c(-2, -OUT_OF_BOUNDS_INT), , drop = FALSE], TST_B[c(-2, -OUT_OF_BOUNDS_INT), , drop = FALSE])

    expect_equal(TST_A[, c(-2, -OUT_OF_BOUNDS_INT)], TST_B[, c(-2, -OUT_OF_BOUNDS_INT)])
    expect_equal(TST_A[, c(-2, -OUT_OF_BOUNDS_INT), drop = TRUE], TST_B[, c(-2, -OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_equal(TST_A[, c(-2, -OUT_OF_BOUNDS_INT), drop = FALSE], TST_B[, c(-2, -OUT_OF_BOUNDS_INT), drop = FALSE])

    expect_equal(TST_A[c(-2, -OUT_OF_BOUNDS_INT), c(-2, -OUT_OF_BOUNDS_INT)], TST_B[c(-2, -OUT_OF_BOUNDS_INT), c(-2, -OUT_OF_BOUNDS_INT)])
    expect_equal(TST_A[c(-2, -OUT_OF_BOUNDS_INT), c(-2, -OUT_OF_BOUNDS_INT), drop = TRUE], TST_B[c(-2, -OUT_OF_BOUNDS_INT), c(-2, -OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_equal(TST_A[c(-2, -OUT_OF_BOUNDS_INT), c(-2, -OUT_OF_BOUNDS_INT), drop = FALSE], TST_B[c(-2, -OUT_OF_BOUNDS_INT), c(-2, -OUT_OF_BOUNDS_INT), drop = FALSE])

    expect_error(TST_A[c(-1, NA), ])
    expect_error(TST_B[c(-1, NA), ])
    expect_error(TST_A[c(-1, NA), , drop = TRUE])
    expect_error(TST_B[c(-1, NA), , drop = TRUE])
    expect_error(TST_A[c(-1, NA), , drop = FALSE])
    expect_error(TST_B[c(-1, NA), , drop = FALSE])

    expect_error(TST_A[, c(-1, NA)])
    expect_error(TST_B[, c(-1, NA)])
    expect_error(TST_A[, c(-1, NA), drop = TRUE])
    expect_error(TST_B[, c(-1, NA), drop = TRUE])
    expect_error(TST_A[, c(-1, NA), drop = FALSE])
    expect_error(TST_B[, c(-1, NA), drop = FALSE])

    expect_error(TST_A[c(-1, NA), c(-1, NA)])
    expect_error(TST_B[c(-1, NA), c(-1, NA)])
    expect_error(TST_A[c(-1, NA), c(-1, NA), drop = TRUE])
    expect_error(TST_B[c(-1, NA), c(-1, NA), drop = TRUE])
    expect_error(TST_A[c(-1, NA), c(-1, NA), drop = FALSE])
    expect_error(TST_B[c(-1, NA), c(-1, NA), drop = FALSE])

})

test_that("multi indexing by logicals works", {

    expect_equal(TST_A[c(TRUE), ], TST_B[c(TRUE), ])
    expect_equal(TST_A[c(TRUE), , drop = TRUE], TST_B[c(TRUE), , drop = TRUE])
    expect_equal(TST_A[c(TRUE), , drop = FALSE], TST_B[c(TRUE), , drop = FALSE])

    expect_equal(TST_A[, c(TRUE)], TST_B[, c(TRUE)])
    expect_equal(TST_A[, c(TRUE), drop = TRUE], TST_B[, c(TRUE), drop = TRUE])
    expect_equal(TST_A[, c(TRUE), drop = FALSE], TST_B[, c(TRUE), drop = FALSE])

    expect_equal(TST_A[c(TRUE), c(TRUE)], TST_B[c(TRUE), c(TRUE)])
    expect_equal(TST_A[c(TRUE), c(TRUE), drop = TRUE], TST_B[c(TRUE), c(TRUE), drop = TRUE])
    expect_equal(TST_A[c(TRUE), c(TRUE), drop = FALSE], TST_B[c(TRUE), c(TRUE), drop = FALSE])

    expect_error(TST_A[c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), ])
    expect_error(TST_B[c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), ])
    expect_error(TST_A[c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), , drop = rep_len(TRUE, OUT_OF_BOUNDS_INT)])
    expect_error(TST_B[c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), , drop = rep_len(TRUE, OUT_OF_BOUNDS_INT)])
    expect_error(TST_A[c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), , drop = FALSE])
    expect_error(TST_B[c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), , drop = FALSE])

    expect_error(TST_A[, c(rep_len(TRUE, OUT_OF_BOUNDS_INT))])
    expect_error(TST_B[, c(rep_len(TRUE, OUT_OF_BOUNDS_INT))])
    expect_error(TST_A[, c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), drop = rep_len(TRUE, OUT_OF_BOUNDS_INT)])
    expect_error(TST_B[, c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), drop = rep_len(TRUE, OUT_OF_BOUNDS_INT)])
    expect_error(TST_A[, c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), drop = FALSE])
    expect_error(TST_B[, c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), drop = FALSE])

    expect_error(TST_A[c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), c(rep_len(TRUE, OUT_OF_BOUNDS_INT))])
    expect_error(TST_B[c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), c(rep_len(TRUE, OUT_OF_BOUNDS_INT))])
    expect_error(TST_A[c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), drop = rep_len(TRUE, OUT_OF_BOUNDS_INT)])
    expect_error(TST_B[c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), drop = rep_len(TRUE, OUT_OF_BOUNDS_INT)])
    expect_error(TST_A[c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), drop = FALSE])
    expect_error(TST_B[c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), drop = FALSE])

    expect_equal(TST_A[c(FALSE), ], TST_B[c(FALSE), ])
    expect_equal(TST_A[c(FALSE), , drop = FALSE], TST_B[c(FALSE), , drop = FALSE])
    expect_equal(TST_A[c(FALSE), , drop = FALSE], TST_B[c(FALSE), , drop = FALSE])

    expect_equal(TST_A[, c(FALSE)], TST_B[, c(FALSE)])
    expect_equal(TST_A[, c(FALSE), drop = FALSE], TST_B[, c(FALSE), drop = FALSE])
    expect_equal(TST_A[, c(FALSE), drop = FALSE], TST_B[, c(FALSE), drop = FALSE])

    expect_equal(TST_A[c(FALSE), c(FALSE)], TST_B[c(FALSE), c(FALSE)])
    expect_equal(TST_A[c(FALSE), c(FALSE), drop = FALSE], TST_B[c(FALSE), c(FALSE), drop = FALSE])
    expect_equal(TST_A[c(FALSE), c(FALSE), drop = FALSE], TST_B[c(FALSE), c(FALSE), drop = FALSE])

    expect_error(TST_A[c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), ])
    expect_error(TST_B[c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), ])
    expect_error(TST_A[c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), , drop = rep_len(FALSE, OUT_OF_BOUNDS_INT)])
    expect_error(TST_B[c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), , drop = rep_len(FALSE, OUT_OF_BOUNDS_INT)])
    expect_error(TST_A[c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), , drop = rep_len(FALSE, OUT_OF_BOUNDS_INT)])
    expect_error(TST_B[c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), , drop = rep_len(FALSE, OUT_OF_BOUNDS_INT)])

    expect_error(TST_A[, c(rep_len(FALSE, OUT_OF_BOUNDS_INT))])
    expect_error(TST_B[, c(rep_len(FALSE, OUT_OF_BOUNDS_INT))])
    expect_error(TST_A[, c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), drop = rep_len(FALSE, OUT_OF_BOUNDS_INT)])
    expect_error(TST_B[, c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), drop = rep_len(FALSE, OUT_OF_BOUNDS_INT)])
    expect_error(TST_A[, c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), drop = rep_len(FALSE, OUT_OF_BOUNDS_INT)])
    expect_error(TST_B[, c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), drop = rep_len(FALSE, OUT_OF_BOUNDS_INT)])

    expect_error(TST_A[c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), c(rep_len(FALSE, OUT_OF_BOUNDS_INT))])
    expect_error(TST_B[c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), c(rep_len(FALSE, OUT_OF_BOUNDS_INT))])
    expect_error(TST_A[c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), drop = rep_len(FALSE, OUT_OF_BOUNDS_INT)])
    expect_error(TST_B[c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), drop = rep_len(FALSE, OUT_OF_BOUNDS_INT)])
    expect_error(TST_A[c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), drop = rep_len(FALSE, OUT_OF_BOUNDS_INT)])
    expect_error(TST_B[c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), drop = rep_len(FALSE, OUT_OF_BOUNDS_INT)])

    expect_equal(TST_A[c(TRUE, FALSE), ], TST_B[c(TRUE, FALSE), ])
    expect_equal(TST_A[c(TRUE, FALSE), , drop = TRUE], TST_B[c(TRUE, FALSE), , drop = TRUE])
    expect_equal(TST_A[c(TRUE, FALSE), , drop = FALSE], TST_B[c(TRUE, FALSE), , drop = FALSE])

    expect_equal(TST_A[, c(TRUE, FALSE)], TST_B[, c(TRUE, FALSE)])
    expect_equal(TST_A[, c(TRUE, FALSE), drop = TRUE], TST_B[, c(TRUE, FALSE), drop = TRUE])
    expect_equal(TST_A[, c(TRUE, FALSE), drop = FALSE], TST_B[, c(TRUE, FALSE), drop = FALSE])

    expect_equal(TST_A[c(TRUE, FALSE), c(TRUE, FALSE)], TST_B[c(TRUE, FALSE), c(TRUE, FALSE)])
    expect_equal(TST_A[c(TRUE, FALSE), c(TRUE, FALSE), drop = TRUE], TST_B[c(TRUE, FALSE), c(TRUE, FALSE), drop = TRUE])
    expect_equal(TST_A[c(TRUE, FALSE), c(TRUE, FALSE), drop = FALSE], TST_B[c(TRUE, FALSE), c(TRUE, FALSE), drop = FALSE])

    expect_error(TST_A[rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), ])
    expect_error(TST_B[rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), ])
    expect_error(TST_A[rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), , drop = TRUE])
    expect_error(TST_B[rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), , drop = TRUE])
    expect_error(TST_A[rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), , drop = FALSE])
    expect_error(TST_B[rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), , drop = FALSE])

    expect_error(TST_A[, rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT)])
    expect_error(TST_B[, rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT)])
    expect_error(TST_A[, rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_error(TST_B[, rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_error(TST_A[, rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = FALSE])
    expect_error(TST_B[, rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = FALSE])

    expect_error(TST_A[rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT)])
    expect_error(TST_B[rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT)])
    expect_error(TST_A[rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_error(TST_B[rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_error(TST_A[rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = FALSE])
    expect_error(TST_B[rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), drop = FALSE])

    expect_equal(TST_A[c(FALSE, TRUE), ], TST_B[c(FALSE, TRUE), ])
    expect_equal(TST_A[c(FALSE, TRUE), , drop = TRUE], TST_B[c(FALSE, TRUE), , drop = TRUE])
    expect_equal(TST_A[c(FALSE, TRUE), , drop = FALSE], TST_B[c(FALSE, TRUE), , drop = FALSE])

    expect_equal(TST_A[, c(FALSE, TRUE)], TST_B[, c(FALSE, TRUE)])
    expect_equal(TST_A[, c(FALSE, TRUE), drop = TRUE], TST_B[, c(FALSE, TRUE), drop = TRUE])
    expect_equal(TST_A[, c(FALSE, TRUE), drop = FALSE], TST_B[, c(FALSE, TRUE), drop = FALSE])

    expect_equal(TST_A[c(FALSE, TRUE), c(FALSE, TRUE)], TST_B[c(FALSE, TRUE), c(FALSE, TRUE)])
    expect_equal(TST_A[c(FALSE, TRUE), c(FALSE, TRUE), drop = TRUE], TST_B[c(FALSE, TRUE), c(FALSE, TRUE), drop = TRUE])
    expect_equal(TST_A[c(FALSE, TRUE), c(FALSE, TRUE), drop = FALSE], TST_B[c(FALSE, TRUE), c(FALSE, TRUE), drop = FALSE])

    expect_error(TST_A[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), ])
    expect_error(TST_B[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), ])
    expect_error(TST_A[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), , drop = TRUE])
    expect_error(TST_B[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), , drop = TRUE])
    expect_error(TST_A[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), , drop = FALSE])
    expect_error(TST_B[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), , drop = FALSE])

    expect_error(TST_A[, rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT)])
    expect_error(TST_B[, rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT)])
    expect_error(TST_A[, rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_error(TST_B[, rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_error(TST_A[, rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = FALSE])
    expect_error(TST_B[, rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = FALSE])

    expect_error(TST_A[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT)])
    expect_error(TST_B[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT)])
    expect_error(TST_A[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_error(TST_B[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = TRUE])
    expect_error(TST_A[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = FALSE])
    expect_error(TST_B[rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), drop = FALSE])

    expect_equal(TST_A[c(TRUE, NA), ], TST_B[c(TRUE, NA), ])
    expect_equal(TST_A[c(TRUE, NA), , drop = TRUE], TST_B[c(TRUE, NA), , drop = TRUE])
    expect_equal(TST_A[c(TRUE, NA), , drop = FALSE], TST_B[c(TRUE, NA), , drop = FALSE])

    expect_equal(TST_A[, c(TRUE, NA)], TST_B[, c(TRUE, NA)])
    expect_equal(TST_A[, c(TRUE, NA), drop = TRUE], TST_B[, c(TRUE, NA), drop = TRUE])
    expect_equal(TST_A[, c(TRUE, NA), drop = FALSE], TST_B[, c(TRUE, NA), drop = FALSE])

    expect_equal(TST_A[c(TRUE, NA), c(TRUE, NA)], TST_B[c(TRUE, NA), c(TRUE, NA)])
    expect_equal(TST_A[c(TRUE, NA), c(TRUE, NA), drop = TRUE], TST_B[c(TRUE, NA), c(TRUE, NA), drop = TRUE])
    expect_equal(TST_A[c(TRUE, NA), c(TRUE, NA), drop = FALSE], TST_B[c(TRUE, NA), c(TRUE, NA), drop = FALSE])

    expect_equal(TST_A[c(FALSE, NA), ], TST_B[c(FALSE, NA), ])
    expect_equal(TST_A[c(FALSE, NA), , drop = FALSE], TST_B[c(FALSE, NA), , drop = FALSE])
    expect_equal(TST_A[c(FALSE, NA), , drop = FALSE], TST_B[c(FALSE, NA), , drop = FALSE])

    expect_equal(TST_A[, c(FALSE, NA)], TST_B[, c(FALSE, NA)])
    expect_equal(TST_A[, c(FALSE, NA), drop = FALSE], TST_B[, c(FALSE, NA), drop = FALSE])
    expect_equal(TST_A[, c(FALSE, NA), drop = FALSE], TST_B[, c(FALSE, NA), drop = FALSE])

    expect_equal(TST_A[c(FALSE, NA), c(FALSE, NA)], TST_B[c(FALSE, NA), c(FALSE, NA)])
    expect_equal(TST_A[c(FALSE, NA), c(FALSE, NA), drop = FALSE], TST_B[c(FALSE, NA), c(FALSE, NA), drop = FALSE])
    expect_equal(TST_A[c(FALSE, NA), c(FALSE, NA), drop = FALSE], TST_B[c(FALSE, NA), c(FALSE, NA), drop = FALSE])

})

test_that("multi indexing by characters works", {

    if (is.null(dimnames(TST_A))) {
        skip("skipping character indexing because dimnames are NULL")
    }

    ROW_NAME_1 <- rownames(TST_A)[1]
    ROW_NAME_2 <- rownames(TST_A)[2]
    COL_NAME_1 <- colnames(TST_A)[1]
    COL_NAME_2 <- colnames(TST_A)[2]

    expect_equal(TST_A[ROW_NAME_1, ], TST_B[ROW_NAME_1, ])
    expect_equal(TST_A[ROW_NAME_1, , drop = TRUE], TST_B[ROW_NAME_1, , drop = TRUE])
    expect_equal(TST_A[ROW_NAME_1, , drop = FALSE], TST_B[ROW_NAME_1, , drop = FALSE])

    expect_equal(TST_A[, COL_NAME_1], TST_B[, COL_NAME_1])
    expect_equal(TST_A[, COL_NAME_1, drop = TRUE], TST_B[, COL_NAME_1, drop = TRUE])
    expect_equal(TST_A[, COL_NAME_1, drop = FALSE], TST_B[, COL_NAME_1, drop = FALSE])

    expect_equal(TST_A[ROW_NAME_1, COL_NAME_1], TST_B[ROW_NAME_1, COL_NAME_1])
    expect_equal(TST_A[ROW_NAME_1, COL_NAME_1, drop = TRUE], TST_B[ROW_NAME_1, COL_NAME_1, drop = TRUE])
    expect_equal(TST_A[ROW_NAME_1, COL_NAME_1, drop = FALSE], TST_B[ROW_NAME_1, COL_NAME_1, drop = FALSE])

    expect_error(TST_A[OUT_OF_BOUNDS_CHAR, ])
    expect_error(TST_B[OUT_OF_BOUNDS_CHAR, ])
    expect_error(TST_A[OUT_OF_BOUNDS_CHAR, , drop = TRUE])
    expect_error(TST_B[OUT_OF_BOUNDS_CHAR, , drop = TRUE])
    expect_error(TST_A[OUT_OF_BOUNDS_CHAR, , drop = FALSE])
    expect_error(TST_B[OUT_OF_BOUNDS_CHAR, , drop = FALSE])

    expect_error(TST_A[, OUT_OF_BOUNDS_CHAR])
    expect_error(TST_B[, OUT_OF_BOUNDS_CHAR])
    expect_error(TST_A[, OUT_OF_BOUNDS_CHAR, drop = TRUE])
    expect_error(TST_B[, OUT_OF_BOUNDS_CHAR, drop = TRUE])
    expect_error(TST_A[, OUT_OF_BOUNDS_CHAR, drop = FALSE])
    expect_error(TST_B[, OUT_OF_BOUNDS_CHAR, drop = FALSE])

    expect_error(TST_A[OUT_OF_BOUNDS_CHAR, OUT_OF_BOUNDS_CHAR])
    expect_error(TST_B[OUT_OF_BOUNDS_CHAR, OUT_OF_BOUNDS_CHAR])
    expect_error(TST_A[OUT_OF_BOUNDS_CHAR, OUT_OF_BOUNDS_CHAR, drop = TRUE])
    expect_error(TST_B[OUT_OF_BOUNDS_CHAR, OUT_OF_BOUNDS_CHAR, drop = TRUE])
    expect_error(TST_A[OUT_OF_BOUNDS_CHAR, OUT_OF_BOUNDS_CHAR, drop = FALSE])
    expect_error(TST_B[OUT_OF_BOUNDS_CHAR, OUT_OF_BOUNDS_CHAR, drop = FALSE])

    expect_equal(TST_A[c(ROW_NAME_1, ROW_NAME_2), ], TST_B[c(ROW_NAME_1, ROW_NAME_2), ])
    expect_equal(TST_A[c(ROW_NAME_1, ROW_NAME_2), , drop = TRUE], TST_B[c(ROW_NAME_1, ROW_NAME_2), , drop = TRUE])
    expect_equal(TST_A[c(ROW_NAME_1, ROW_NAME_2), , drop = FALSE], TST_B[c(ROW_NAME_1, ROW_NAME_2), , drop = FALSE])

    expect_equal(TST_A[, c(COL_NAME_1, COL_NAME_2)], TST_B[, c(COL_NAME_1, COL_NAME_2)])
    expect_equal(TST_A[, c(COL_NAME_1, COL_NAME_2), drop = TRUE], TST_B[, c(COL_NAME_1, COL_NAME_2), drop = TRUE])
    expect_equal(TST_A[, c(COL_NAME_1, COL_NAME_2), drop = FALSE], TST_B[, c(COL_NAME_1, COL_NAME_2), drop = FALSE])

    expect_equal(TST_A[c(ROW_NAME_1, ROW_NAME_2), c(COL_NAME_1, COL_NAME_2)], TST_B[c(ROW_NAME_1, ROW_NAME_2), c(COL_NAME_1, COL_NAME_2)])
    expect_equal(TST_A[c(ROW_NAME_1, ROW_NAME_2), c(COL_NAME_1, COL_NAME_2), drop = TRUE], TST_B[c(ROW_NAME_1, ROW_NAME_2), c(COL_NAME_1, COL_NAME_2), drop = TRUE])
    expect_equal(TST_A[c(ROW_NAME_1, ROW_NAME_2), c(COL_NAME_1, COL_NAME_2), drop = FALSE], TST_B[c(ROW_NAME_1, ROW_NAME_2), c(COL_NAME_1, COL_NAME_2), drop = FALSE])

    expect_equal(TST_A[c(ROW_NAME_2, ROW_NAME_1), ], TST_B[c(ROW_NAME_2, ROW_NAME_1), ])
    expect_equal(TST_A[c(ROW_NAME_2, ROW_NAME_1), , drop = TRUE], TST_B[c(ROW_NAME_2, ROW_NAME_1), , drop = TRUE])
    expect_equal(TST_A[c(ROW_NAME_2, ROW_NAME_1), , drop = FALSE], TST_B[c(ROW_NAME_2, ROW_NAME_1), , drop = FALSE])

    expect_equal(TST_A[, c(COL_NAME_2, COL_NAME_1)], TST_B[, c(COL_NAME_2, COL_NAME_1)])
    expect_equal(TST_A[, c(COL_NAME_2, COL_NAME_1), drop = TRUE], TST_B[, c(COL_NAME_2, COL_NAME_1), drop = TRUE])
    expect_equal(TST_A[, c(COL_NAME_2, COL_NAME_1), drop = FALSE], TST_B[, c(COL_NAME_2, COL_NAME_1), drop = FALSE])

    expect_equal(TST_A[c(ROW_NAME_2, ROW_NAME_1), c(COL_NAME_2, COL_NAME_1)], TST_B[c(ROW_NAME_2, ROW_NAME_1), c(COL_NAME_2, COL_NAME_1)])
    expect_equal(TST_A[c(ROW_NAME_2, ROW_NAME_1), c(COL_NAME_2, COL_NAME_1), drop = TRUE], TST_B[c(ROW_NAME_2, ROW_NAME_1), c(COL_NAME_2, COL_NAME_1), drop = TRUE])
    expect_equal(TST_A[c(ROW_NAME_2, ROW_NAME_1), c(COL_NAME_2, COL_NAME_1), drop = FALSE], TST_B[c(ROW_NAME_2, ROW_NAME_1), c(COL_NAME_2, COL_NAME_1), drop = FALSE])

    expect_error(TST_A[c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), ])
    expect_error(TST_B[c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), ])
    expect_error(TST_A[c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), , drop = TRUE])
    expect_error(TST_B[c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), , drop = TRUE])
    expect_error(TST_A[c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), , drop = FALSE])
    expect_error(TST_B[c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), , drop = FALSE])

    expect_error(TST_A[, c(COL_NAME_1, OUT_OF_BOUNDS_CHAR)])
    expect_error(TST_B[, c(COL_NAME_1, OUT_OF_BOUNDS_CHAR)])
    expect_error(TST_A[, c(COL_NAME_1, OUT_OF_BOUNDS_CHAR), drop = TRUE])
    expect_error(TST_B[, c(COL_NAME_1, OUT_OF_BOUNDS_CHAR), drop = TRUE])
    expect_error(TST_A[, c(COL_NAME_1, OUT_OF_BOUNDS_CHAR), drop = FALSE])
    expect_error(TST_B[, c(COL_NAME_1, OUT_OF_BOUNDS_CHAR), drop = FALSE])

    expect_error(TST_A[c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), c(COL_NAME_1, OUT_OF_BOUNDS_CHAR)])
    expect_error(TST_B[c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), c(COL_NAME_1, OUT_OF_BOUNDS_CHAR)])
    expect_error(TST_A[c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), c(COL_NAME_1, OUT_OF_BOUNDS_CHAR), drop = TRUE])
    expect_error(TST_B[c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), c(COL_NAME_1, OUT_OF_BOUNDS_CHAR), drop = TRUE])
    expect_error(TST_A[c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), c(COL_NAME_1, OUT_OF_BOUNDS_CHAR), drop = FALSE])
    expect_error(TST_B[c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), c(COL_NAME_1, OUT_OF_BOUNDS_CHAR), drop = FALSE])

    expect_error(TST_A[c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), ])
    expect_error(TST_B[c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), ])
    expect_error(TST_A[c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), , drop = TRUE])
    expect_error(TST_B[c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), , drop = TRUE])
    expect_error(TST_A[c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), , drop = FALSE])
    expect_error(TST_B[c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), , drop = FALSE])

    expect_error(TST_A[, c(OUT_OF_BOUNDS_CHAR, COL_NAME_1)])
    expect_error(TST_B[, c(OUT_OF_BOUNDS_CHAR, COL_NAME_1)])
    expect_error(TST_A[, c(OUT_OF_BOUNDS_CHAR, COL_NAME_1), drop = TRUE])
    expect_error(TST_B[, c(OUT_OF_BOUNDS_CHAR, COL_NAME_1), drop = TRUE])
    expect_error(TST_A[, c(OUT_OF_BOUNDS_CHAR, COL_NAME_1), drop = FALSE])
    expect_error(TST_B[, c(OUT_OF_BOUNDS_CHAR, COL_NAME_1), drop = FALSE])

    expect_error(TST_A[c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), c(OUT_OF_BOUNDS_CHAR, COL_NAME_1)])
    expect_error(TST_B[c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), c(OUT_OF_BOUNDS_CHAR, COL_NAME_1)])
    expect_error(TST_A[c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), c(OUT_OF_BOUNDS_CHAR, COL_NAME_1), drop = TRUE])
    expect_error(TST_B[c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), c(OUT_OF_BOUNDS_CHAR, COL_NAME_1), drop = TRUE])
    expect_error(TST_A[c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), c(OUT_OF_BOUNDS_CHAR, COL_NAME_1), drop = FALSE])
    expect_error(TST_B[c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), c(OUT_OF_BOUNDS_CHAR, COL_NAME_1), drop = FALSE])

    expect_error(TST_A[c(ROW_NAME_1, NA), ])
    expect_error(TST_B[c(ROW_NAME_1, NA), ])
    expect_error(TST_A[c(ROW_NAME_1, NA), , drop = TRUE])
    expect_error(TST_B[c(ROW_NAME_1, NA), , drop = TRUE])
    expect_error(TST_A[c(ROW_NAME_1, NA), , drop = FALSE])
    expect_error(TST_B[c(ROW_NAME_1, NA), , drop = FALSE])

    expect_error(TST_A[, c(COL_NAME_1, NA)])
    expect_error(TST_B[, c(COL_NAME_1, NA)])
    expect_error(TST_A[, c(COL_NAME_1, NA), drop = TRUE])
    expect_error(TST_B[, c(COL_NAME_1, NA), drop = TRUE])
    expect_error(TST_A[, c(COL_NAME_1, NA), drop = FALSE])
    expect_error(TST_B[, c(COL_NAME_1, NA), drop = FALSE])

    expect_error(TST_A[c(ROW_NAME_1, NA), c(COL_NAME_1, NA)])
    expect_error(TST_B[c(ROW_NAME_1, NA), c(COL_NAME_1, NA)])
    expect_error(TST_A[c(ROW_NAME_1, NA), c(COL_NAME_1, NA), drop = TRUE])
    expect_error(TST_B[c(ROW_NAME_1, NA), c(COL_NAME_1, NA), drop = TRUE])
    expect_error(TST_A[c(ROW_NAME_1, NA), c(COL_NAME_1, NA), drop = FALSE])
    expect_error(TST_B[c(ROW_NAME_1, NA), c(COL_NAME_1, NA), drop = FALSE])

})

test_that("multi indexing by NA works", {

    expect_equal(TST_A[NA, ], TST_B[NA, ])
    expect_equal(TST_A[NA, , drop = TRUE], TST_B[NA, , drop = TRUE])
    expect_equal(TST_A[NA, , drop = FALSE], TST_B[NA, , drop = FALSE])

    expect_equal(TST_A[, NA], TST_B[, NA])
    expect_equal(TST_A[, NA, drop = TRUE], TST_B[, NA, drop = TRUE])
    expect_equal(TST_A[, NA, drop = FALSE], TST_B[, NA, drop = FALSE])

    expect_equal(TST_A[NA, NA], TST_B[NA, NA])
    expect_equal(TST_A[NA, NA, drop = TRUE], TST_B[NA, NA, drop = TRUE])
    expect_equal(TST_A[NA, NA, drop = FALSE], TST_B[NA, NA, drop = FALSE])

    expect_equal(TST_A[NA_integer_, ], TST_B[NA_integer_, ])
    expect_equal(TST_A[NA_integer_, , drop = TRUE], TST_B[NA_integer_, , drop = TRUE])
    expect_equal(TST_A[NA_integer_, , drop = FALSE], TST_B[NA_integer_, , drop = FALSE])

    expect_equal(TST_A[, NA_integer_], TST_B[, NA_integer_])
    expect_equal(TST_A[, NA_integer_, drop = TRUE], TST_B[, NA_integer_, drop = TRUE])
    expect_equal(TST_A[, NA_integer_, drop = FALSE], TST_B[, NA_integer_, drop = FALSE])

    expect_equal(TST_A[NA_integer_, NA_integer_], TST_B[NA_integer_, NA_integer_])
    expect_equal(TST_A[NA_integer_, NA_integer_, drop = TRUE], TST_B[NA_integer_, NA_integer_, drop = TRUE])
    expect_equal(TST_A[NA_integer_, NA_integer_, drop = FALSE], TST_B[NA_integer_, NA_integer_, drop = FALSE])

    expect_error(TST_A[NA_character_, ])
    expect_error(TST_B[NA_character_, ])
    expect_error(TST_A[NA_character_, , drop = TRUE])
    expect_error(TST_B[NA_character_, , drop = TRUE])
    expect_error(TST_A[NA_character_, , drop = FALSE])
    expect_error(TST_B[NA_character_, , drop = FALSE])

    expect_error(TST_A[, NA_character_])
    expect_error(TST_B[, NA_character_])
    expect_error(TST_A[, NA_character_, drop = TRUE])
    expect_error(TST_B[, NA_character_, drop = TRUE])
    expect_error(TST_A[, NA_character_, drop = FALSE])
    expect_error(TST_B[, NA_character_, drop = FALSE])

    expect_error(TST_A[NA_character_, NA_character_])
    expect_error(TST_B[NA_character_, NA_character_])
    expect_error(TST_A[NA_character_, NA_character_, drop = TRUE])
    expect_error(TST_B[NA_character_, NA_character_, drop = TRUE])
    expect_error(TST_A[NA_character_, NA_character_, drop = FALSE])
    expect_error(TST_B[NA_character_, NA_character_, drop = FALSE])

})

test_that("multi indexing by zero works", {

    expect_error(TST_A[0, 0]) # Not implemented
    expect_error(TST_A[0, 0, drop = TRUE]) # Not implemented
    expect_error(TST_A[0, 0, drop = FALSE]) # Not implemented

    expect_equal(TST_A[c(0, 1), ], TST_B[c(0, 1), ])
    expect_equal(TST_A[c(0, 1), , drop = TRUE], TST_B[c(0, 1), , drop = TRUE])
    expect_equal(TST_A[c(0, 1), , drop = FALSE], TST_B[c(0, 1), , drop = FALSE])

    expect_equal(TST_A[, c(0, 1)], TST_B[, c(0, 1)])
    expect_equal(TST_A[, c(0, 1), drop = TRUE], TST_B[, c(0, 1), drop = TRUE])
    expect_equal(TST_A[, c(0, 1), drop = FALSE], TST_B[, c(0, 1), drop = FALSE])

    expect_equal(TST_A[c(0, 1), c(0, 1)], TST_B[c(0, 1), c(0, 1)])
    expect_equal(TST_A[c(0, 1), c(0, 1), drop = TRUE], TST_B[c(0, 1), c(0, 1), drop = TRUE])
    expect_equal(TST_A[c(0, 1), c(0, 1), drop = FALSE], TST_B[c(0, 1), c(0, 1), drop = FALSE])

    expect_equal(TST_A[c(0, -1), ], TST_B[c(0, -1), ])
    expect_equal(TST_A[c(0, -1), , drop = TRUE], TST_B[c(0, -1), , drop = TRUE])
    expect_equal(TST_A[c(0, -1), , drop = FALSE], TST_B[c(0, -1), , drop = FALSE])

    expect_equal(TST_A[, c(0, -1)], TST_B[, c(0, -1)])
    expect_equal(TST_A[, c(0, -1), drop = TRUE], TST_B[, c(0, -1), drop = TRUE])
    expect_equal(TST_A[, c(0, -1), drop = FALSE], TST_B[, c(0, -1), drop = FALSE])

    expect_equal(TST_A[c(0, -1), c(0, -1)], TST_B[c(0, -1), c(0, -1)])
    expect_equal(TST_A[c(0, -1), c(0, -1), drop = TRUE], TST_B[c(0, -1), c(0, -1), drop = TRUE])
    expect_equal(TST_A[c(0, -1), c(0, -1), drop = FALSE], TST_B[c(0, -1), c(0, -1), drop = FALSE])

    expect_error(TST_A[c(0, 1, -1), ])
    expect_error(TST_B[c(0, 1, -1), ])
    expect_error(TST_A[c(0, 1, -1), , drop = TRUE])
    expect_error(TST_B[c(0, 1, -1), , drop = TRUE])
    expect_error(TST_A[c(0, 1, -1), , drop = FALSE])
    expect_error(TST_B[c(0, 1, -1), , drop = FALSE])

    expect_error(TST_A[, c(0, 1, -1)])
    expect_error(TST_B[, c(0, 1, -1)])
    expect_error(TST_A[, c(0, 1, -1), drop = TRUE])
    expect_error(TST_B[, c(0, 1, -1), drop = TRUE])
    expect_error(TST_A[, c(0, 1, -1), drop = FALSE])
    expect_error(TST_B[, c(0, 1, -1), drop = FALSE])

    expect_error(TST_A[c(0, 1, -1), c(0, 1, -1)])
    expect_error(TST_B[c(0, 1, -1), c(0, 1, -1)])
    expect_error(TST_A[c(0, 1, -1), c(0, 1, -1), drop = TRUE])
    expect_error(TST_B[c(0, 1, -1), c(0, 1, -1), drop = TRUE])
    expect_error(TST_A[c(0, 1, -1), c(0, 1, -1), drop = FALSE])
    expect_error(TST_B[c(0, 1, -1), c(0, 1, -1), drop = FALSE])

})
