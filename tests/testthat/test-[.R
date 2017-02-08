test_that("dimnames are the same", {

    expect_equal(dimnames(TST_A), dimnames(TST_B))

})

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

    expect_equal(TST_A[100], TST_B[100])
    expect_equal(TST_A[100, drop = TRUE], TST_B[100, drop = TRUE])
    expect_equal(TST_A[100, drop = FALSE], TST_B[100, drop = FALSE])

    expect_equal(TST_A[c(100, 2)], TST_B[c(100, 2)])
    expect_equal(TST_A[c(100, 2), drop = TRUE], TST_B[c(100, 2), drop = TRUE])
    expect_equal(TST_A[c(100, 2), drop = FALSE], TST_B[c(100, 2), drop = FALSE])

    expect_equal(TST_A[c(2, 100)], TST_B[c(2, 100)])
    expect_equal(TST_A[c(2, 100), drop = TRUE], TST_B[c(2, 100), drop = TRUE])
    expect_equal(TST_A[c(2, 100), drop = FALSE], TST_B[c(2, 100), drop = FALSE])

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

    m <- matrix(data = c(100, 100), ncol = 2, byrow = TRUE)
    expect_error(TST_A[m])
    expect_error(TST_B[m])
    expect_error(TST_A[m, drop = TRUE])
    expect_error(TST_B[m, drop = TRUE])
    expect_error(TST_A[m, drop = FALSE])
    expect_error(TST_B[m, drop = FALSE])

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

    expect_equal(TST_A[-100], TST_B[-100])
    expect_equal(TST_A[-100, drop = TRUE], TST_B[-100, drop = TRUE])
    expect_equal(TST_A[-100, drop = FALSE], TST_B[-100, drop = FALSE])

    expect_equal(TST_A[c(-100, -2)], TST_B[c(-100, -2)])
    expect_equal(TST_A[c(-100, -2), drop = TRUE], TST_B[c(-100, -2), drop = TRUE])
    expect_equal(TST_A[c(-100, -2), drop = FALSE], TST_B[c(-100, -2), drop = FALSE])

    expect_equal(TST_A[c(-2, -100)], TST_B[c(-2, -100)])
    expect_equal(TST_A[c(-2, -100), drop = TRUE], TST_B[c(-2, -100), drop = TRUE])
    expect_equal(TST_A[c(-2, -100), drop = FALSE], TST_B[c(-2, -100), drop = FALSE])

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

    expect_equal(TST_A[rep_len(TRUE, 100)], TST_B[rep_len(TRUE, 100)])
    expect_equal(TST_A[rep_len(TRUE, 100), drop = rep_len(TRUE, 100)], TST_B[rep_len(TRUE, 100), drop = rep_len(TRUE, 100)])
    expect_equal(TST_A[rep_len(TRUE, 100), drop = FALSE], TST_B[rep_len(TRUE, 100), drop = FALSE])

    expect_equal(TST_A[rep_len(FALSE, 100)], TST_B[rep_len(FALSE, 100)])
    expect_equal(TST_A[rep_len(FALSE, 100), drop = rep_len(FALSE, 100)], TST_B[rep_len(FALSE, 100), drop = rep_len(FALSE, 100)])
    expect_equal(TST_A[rep_len(FALSE, 100), drop = rep_len(FALSE, 100)], TST_B[rep_len(FALSE, 100), drop = rep_len(FALSE, 100)])

    expect_equal(TST_A[rep_len(c(TRUE, FALSE), 100)], TST_B[rep_len(c(TRUE, FALSE), 100)])
    expect_equal(TST_A[rep_len(c(TRUE, FALSE), 100), drop = TRUE], TST_B[rep_len(c(TRUE, FALSE), 100), drop = TRUE])
    expect_equal(TST_A[rep_len(c(TRUE, FALSE), 100), drop = FALSE], TST_B[rep_len(c(TRUE, FALSE), 100), drop = FALSE])

    expect_equal(TST_A[rep_len(c(FALSE, TRUE), 100)], TST_B[rep_len(c(FALSE, TRUE), 100)])
    expect_equal(TST_A[rep_len(c(FALSE, TRUE), 100), drop = TRUE], TST_B[rep_len(c(FALSE, TRUE), 100), drop = TRUE])
    expect_equal(TST_A[rep_len(c(FALSE, TRUE), 100), drop = FALSE], TST_B[rep_len(c(FALSE, TRUE), 100), drop = FALSE])

    m <- matrix(data = rnorm(25), nrow = 5, ncol = 5)
    expect_equal(TST_A[m > 1], TST_B[m > 1])
    expect_equal(TST_A[m > 1, drop = TRUE], TST_B[m > 1, drop = TRUE])
    expect_equal(TST_A[m > 1, drop = FALSE], TST_B[m > 1, drop = FALSE])

})

test_that("single indexing by characters works", {

    if (is.null(dimnames(TST_A))) {
        skip("skipping character indexing because dimnames are NULL")
    }

    expect_equal(TST_A["a"], TST_B["a"])
    expect_equal(TST_A["a", drop = TRUE], TST_B["a", drop = TRUE])
    expect_equal(TST_A["a", drop = FALSE], TST_B["a", drop = FALSE])

    expect_equal(TST_A["x"], TST_B["x"])
    expect_equal(TST_A["x", drop = TRUE], TST_B["x", drop = TRUE])
    expect_equal(TST_A["x", drop = FALSE], TST_B["x", drop = FALSE])

    m <- matrix(data = c("a", "a", "b", "b"), ncol = 2, byrow = TRUE)
    expect_equal(TST_A[m], TST_B[m])
    expect_equal(TST_A[m, drop = TRUE], TST_B[m, drop = TRUE])
    expect_equal(TST_A[m, drop = FALSE], TST_B[m, drop = FALSE])

    m <- matrix(data = c("b", "b", "a", "a"), ncol = 2, byrow = TRUE)
    expect_equal(TST_A[m], TST_B[m])
    expect_equal(TST_A[m, drop = TRUE], TST_B[m, drop = TRUE])
    expect_equal(TST_A[m, drop = FALSE], TST_B[m, drop = FALSE])

    m <- matrix(data = c("x", "x"), ncol = 2, byrow = TRUE)
    expect_error(TST_A[m])
    expect_error(TST_B[m])
    expect_error(TST_A[m, drop = TRUE])
    expect_error(TST_B[m, drop = TRUE])
    expect_error(TST_A[m, drop = FALSE])
    expect_error(TST_B[m, drop = FALSE])

})

test_that("single indexing by NA works", {

    expect_equal(TST_A[c(1, NA)], TST_B[c(1, NA)])
    expect_equal(TST_A[c(1, NA), drop = TRUE], TST_B[c(1, NA), drop = TRUE])
    expect_equal(TST_A[c(1, NA), drop = FALSE], TST_B[c(1, NA), drop = FALSE])

    expect_error(TST_A[c(-1, NA)])
    expect_error(TST_B[c(-1, NA)])
    expect_error(TST_A[c(-1, NA), drop = TRUE])
    expect_error(TST_B[c(-1, NA), drop = TRUE])
    expect_error(TST_A[c(-1, NA), drop = FALSE])
    expect_error(TST_B[c(-1, NA), drop = FALSE])

    expect_equal(TST_A[c("a", NA)], TST_B[c("a", NA)])
    expect_equal(TST_A[c("a", NA), drop = TRUE], TST_B[c("a", NA), drop = TRUE])
    expect_equal(TST_A[c("a", NA), drop = FALSE], TST_B[c("a", NA), drop = FALSE])

    expect_equal(TST_A[c(TRUE, NA)], TST_B[c(TRUE, NA)])
    expect_equal(TST_A[c(TRUE, NA), drop = TRUE], TST_B[c(TRUE, NA), drop = TRUE])
    expect_equal(TST_A[c(TRUE, NA), drop = FALSE], TST_B[c(TRUE, NA), drop = FALSE])

    expect_equal(TST_A[c(FALSE, NA)], TST_B[c(FALSE, NA)])
    expect_equal(TST_A[c(FALSE, NA), drop = TRUE], TST_B[c(FALSE, NA), drop = TRUE])
    expect_equal(TST_A[c(FALSE, NA), drop = FALSE], TST_B[c(FALSE, NA), drop = FALSE])

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

    expect_error(TST_A[100, ])
    expect_error(TST_B[100, ])
    expect_error(TST_A[100, , drop = TRUE])
    expect_error(TST_B[100, , drop = TRUE])
    expect_error(TST_A[100, , drop = FALSE])
    expect_error(TST_B[100, , drop = FALSE])

    expect_error(TST_A[, 100])
    expect_error(TST_B[, 100])
    expect_error(TST_A[, 100, drop = TRUE])
    expect_error(TST_B[, 100, drop = TRUE])
    expect_error(TST_A[, 100, drop = FALSE])
    expect_error(TST_B[, 100, drop = FALSE])

    expect_error(TST_A[100, 100])
    expect_error(TST_B[100, 100])
    expect_error(TST_A[100, 100, drop = TRUE])
    expect_error(TST_B[100, 100, drop = TRUE])
    expect_error(TST_A[100, 100, drop = FALSE])
    expect_error(TST_B[100, 100, drop = FALSE])

    expect_error(TST_A[c(100, 2), ])
    expect_error(TST_B[c(100, 2), ])
    expect_error(TST_A[c(100, 2), , drop = TRUE])
    expect_error(TST_B[c(100, 2), , drop = TRUE])
    expect_error(TST_A[c(100, 2), , drop = FALSE])
    expect_error(TST_B[c(100, 2), , drop = FALSE])

    expect_error(TST_A[, c(100, 2)])
    expect_error(TST_B[, c(100, 2)])
    expect_error(TST_A[, c(100, 2), drop = TRUE])
    expect_error(TST_B[, c(100, 2), drop = TRUE])
    expect_error(TST_A[, c(100, 2), drop = FALSE])
    expect_error(TST_B[, c(100, 2), drop = FALSE])

    expect_error(TST_A[c(100, 2), c(100, 2)])
    expect_error(TST_B[c(100, 2), c(100, 2)])
    expect_error(TST_A[c(100, 2), c(100, 2), drop = TRUE])
    expect_error(TST_B[c(100, 2), c(100, 2), drop = TRUE])
    expect_error(TST_A[c(100, 2), c(100, 2), drop = FALSE])
    expect_error(TST_B[c(100, 2), c(100, 2), drop = FALSE])

    expect_error(TST_A[c(2, 100), ])
    expect_error(TST_B[c(2, 100), ])
    expect_error(TST_A[c(2, 100), , drop = TRUE])
    expect_error(TST_B[c(2, 100), , drop = TRUE])
    expect_error(TST_A[c(2, 100), , drop = FALSE])
    expect_error(TST_B[c(2, 100), , drop = FALSE])

    expect_error(TST_A[, c(2, 100)])
    expect_error(TST_B[, c(2, 100)])
    expect_error(TST_A[, c(2, 100), drop = TRUE])
    expect_error(TST_B[, c(2, 100), drop = TRUE])
    expect_error(TST_A[, c(2, 100), drop = FALSE])
    expect_error(TST_B[, c(2, 100), drop = FALSE])

    expect_error(TST_A[c(2, 100), c(2, 100)])
    expect_error(TST_B[c(2, 100), c(2, 100)])
    expect_error(TST_A[c(2, 100), c(2, 100), drop = TRUE])
    expect_error(TST_B[c(2, 100), c(2, 100), drop = TRUE])
    expect_error(TST_A[c(2, 100), c(2, 100), drop = FALSE])
    expect_error(TST_B[c(2, 100), c(2, 100), drop = FALSE])

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

    expect_equal(TST_A[c(-100), ], TST_B[c(-100), ])
    expect_equal(TST_A[c(-100), , drop = TRUE], TST_B[c(-100), , drop = TRUE])
    expect_equal(TST_A[c(-100), , drop = FALSE], TST_B[c(-100), , drop = FALSE])

    expect_equal(TST_A[, c(-100)], TST_B[, c(-100)])
    expect_equal(TST_A[, c(-100), drop = TRUE], TST_B[, c(-100), drop = TRUE])
    expect_equal(TST_A[, c(-100), drop = FALSE], TST_B[, c(-100), drop = FALSE])

    expect_equal(TST_A[c(-100), c(-100)], TST_B[c(-100), c(-100)])
    expect_equal(TST_A[c(-100), c(-100), drop = TRUE], TST_B[c(-100), c(-100), drop = TRUE])
    expect_equal(TST_A[c(-100), c(-100), drop = FALSE], TST_B[c(-100), c(-100), drop = FALSE])

    expect_equal(TST_A[c(-100, -2), ], TST_B[c(-100, -2), ])
    expect_equal(TST_A[c(-100, -2), , drop = TRUE], TST_B[c(-100, -2), , drop = TRUE])
    expect_equal(TST_A[c(-100, -2), , drop = FALSE], TST_B[c(-100, -2), , drop = FALSE])

    expect_equal(TST_A[, c(-100, -2)], TST_B[, c(-100, -2)])
    expect_equal(TST_A[, c(-100, -2), drop = TRUE], TST_B[, c(-100, -2), drop = TRUE])
    expect_equal(TST_A[, c(-100, -2), drop = FALSE], TST_B[, c(-100, -2), drop = FALSE])

    expect_equal(TST_A[c(-100, -2), c(-100, -2)], TST_B[c(-100, -2), c(-100, -2)])
    expect_equal(TST_A[c(-100, -2), c(-100, -2), drop = TRUE], TST_B[c(-100, -2), c(-100, -2), drop = TRUE])
    expect_equal(TST_A[c(-100, -2), c(-100, -2), drop = FALSE], TST_B[c(-100, -2), c(-100, -2), drop = FALSE])

    expect_equal(TST_A[c(-2, -100), ], TST_B[c(-2, -100), ])
    expect_equal(TST_A[c(-2, -100), , drop = TRUE], TST_B[c(-2, -100), , drop = TRUE])
    expect_equal(TST_A[c(-2, -100), , drop = FALSE], TST_B[c(-2, -100), , drop = FALSE])

    expect_equal(TST_A[, c(-2, -100)], TST_B[, c(-2, -100)])
    expect_equal(TST_A[, c(-2, -100), drop = TRUE], TST_B[, c(-2, -100), drop = TRUE])
    expect_equal(TST_A[, c(-2, -100), drop = FALSE], TST_B[, c(-2, -100), drop = FALSE])

    expect_equal(TST_A[c(-2, -100), c(-2, -100)], TST_B[c(-2, -100), c(-2, -100)])
    expect_equal(TST_A[c(-2, -100), c(-2, -100), drop = TRUE], TST_B[c(-2, -100), c(-2, -100), drop = TRUE])
    expect_equal(TST_A[c(-2, -100), c(-2, -100), drop = FALSE], TST_B[c(-2, -100), c(-2, -100), drop = FALSE])

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

    expect_error(TST_A[c(rep_len(TRUE, 100)), ])
    expect_error(TST_B[c(rep_len(TRUE, 100)), ])
    expect_error(TST_A[c(rep_len(TRUE, 100)), , drop = rep_len(TRUE, 100)])
    expect_error(TST_B[c(rep_len(TRUE, 100)), , drop = rep_len(TRUE, 100)])
    expect_error(TST_A[c(rep_len(TRUE, 100)), , drop = FALSE])
    expect_error(TST_B[c(rep_len(TRUE, 100)), , drop = FALSE])

    expect_error(TST_A[, c(rep_len(TRUE, 100))])
    expect_error(TST_B[, c(rep_len(TRUE, 100))])
    expect_error(TST_A[, c(rep_len(TRUE, 100)), drop = rep_len(TRUE, 100)])
    expect_error(TST_B[, c(rep_len(TRUE, 100)), drop = rep_len(TRUE, 100)])
    expect_error(TST_A[, c(rep_len(TRUE, 100)), drop = FALSE])
    expect_error(TST_B[, c(rep_len(TRUE, 100)), drop = FALSE])

    expect_error(TST_A[c(rep_len(TRUE, 100)), c(rep_len(TRUE, 100))])
    expect_error(TST_B[c(rep_len(TRUE, 100)), c(rep_len(TRUE, 100))])
    expect_error(TST_A[c(rep_len(TRUE, 100)), c(rep_len(TRUE, 100)), drop = rep_len(TRUE, 100)])
    expect_error(TST_B[c(rep_len(TRUE, 100)), c(rep_len(TRUE, 100)), drop = rep_len(TRUE, 100)])
    expect_error(TST_A[c(rep_len(TRUE, 100)), c(rep_len(TRUE, 100)), drop = FALSE])
    expect_error(TST_B[c(rep_len(TRUE, 100)), c(rep_len(TRUE, 100)), drop = FALSE])

    expect_equal(TST_A[c(FALSE), ], TST_B[c(FALSE), ])
    expect_equal(TST_A[c(FALSE), , drop = FALSE], TST_B[c(FALSE), , drop = FALSE])
    expect_equal(TST_A[c(FALSE), , drop = FALSE], TST_B[c(FALSE), , drop = FALSE])

    expect_equal(TST_A[, c(FALSE)], TST_B[, c(FALSE)])
    expect_equal(TST_A[, c(FALSE), drop = FALSE], TST_B[, c(FALSE), drop = FALSE])
    expect_equal(TST_A[, c(FALSE), drop = FALSE], TST_B[, c(FALSE), drop = FALSE])

    expect_equal(TST_A[c(FALSE), c(FALSE)], TST_B[c(FALSE), c(FALSE)])
    expect_equal(TST_A[c(FALSE), c(FALSE), drop = FALSE], TST_B[c(FALSE), c(FALSE), drop = FALSE])
    expect_equal(TST_A[c(FALSE), c(FALSE), drop = FALSE], TST_B[c(FALSE), c(FALSE), drop = FALSE])

    expect_error(TST_A[c(rep_len(FALSE, 100)), ])
    expect_error(TST_B[c(rep_len(FALSE, 100)), ])
    expect_error(TST_A[c(rep_len(FALSE, 100)), , drop = rep_len(FALSE, 100)])
    expect_error(TST_B[c(rep_len(FALSE, 100)), , drop = rep_len(FALSE, 100)])
    expect_error(TST_A[c(rep_len(FALSE, 100)), , drop = rep_len(FALSE, 100)])
    expect_error(TST_B[c(rep_len(FALSE, 100)), , drop = rep_len(FALSE, 100)])

    expect_error(TST_A[, c(rep_len(FALSE, 100))])
    expect_error(TST_B[, c(rep_len(FALSE, 100))])
    expect_error(TST_A[, c(rep_len(FALSE, 100)), drop = rep_len(FALSE, 100)])
    expect_error(TST_B[, c(rep_len(FALSE, 100)), drop = rep_len(FALSE, 100)])
    expect_error(TST_A[, c(rep_len(FALSE, 100)), drop = rep_len(FALSE, 100)])
    expect_error(TST_B[, c(rep_len(FALSE, 100)), drop = rep_len(FALSE, 100)])

    expect_error(TST_A[c(rep_len(FALSE, 100)), c(rep_len(FALSE, 100))])
    expect_error(TST_B[c(rep_len(FALSE, 100)), c(rep_len(FALSE, 100))])
    expect_error(TST_A[c(rep_len(FALSE, 100)), c(rep_len(FALSE, 100)), drop = rep_len(FALSE, 100)])
    expect_error(TST_B[c(rep_len(FALSE, 100)), c(rep_len(FALSE, 100)), drop = rep_len(FALSE, 100)])
    expect_error(TST_A[c(rep_len(FALSE, 100)), c(rep_len(FALSE, 100)), drop = rep_len(FALSE, 100)])
    expect_error(TST_B[c(rep_len(FALSE, 100)), c(rep_len(FALSE, 100)), drop = rep_len(FALSE, 100)])

    expect_equal(TST_A[c(TRUE, FALSE), ], TST_B[c(TRUE, FALSE), ])
    expect_equal(TST_A[c(TRUE, FALSE), , drop = TRUE], TST_B[c(TRUE, FALSE), , drop = TRUE])
    expect_equal(TST_A[c(TRUE, FALSE), , drop = FALSE], TST_B[c(TRUE, FALSE), , drop = FALSE])

    expect_equal(TST_A[, c(TRUE, FALSE)], TST_B[, c(TRUE, FALSE)])
    expect_equal(TST_A[, c(TRUE, FALSE), drop = TRUE], TST_B[, c(TRUE, FALSE), drop = TRUE])
    expect_equal(TST_A[, c(TRUE, FALSE), drop = FALSE], TST_B[, c(TRUE, FALSE), drop = FALSE])

    expect_equal(TST_A[c(TRUE, FALSE), c(TRUE, FALSE)], TST_B[c(TRUE, FALSE), c(TRUE, FALSE)])
    expect_equal(TST_A[c(TRUE, FALSE), c(TRUE, FALSE), drop = TRUE], TST_B[c(TRUE, FALSE), c(TRUE, FALSE), drop = TRUE])
    expect_equal(TST_A[c(TRUE, FALSE), c(TRUE, FALSE), drop = FALSE], TST_B[c(TRUE, FALSE), c(TRUE, FALSE), drop = FALSE])

    expect_error(TST_A[rep_len(c(TRUE, FALSE), 100), ])
    expect_error(TST_B[rep_len(c(TRUE, FALSE), 100), ])
    expect_error(TST_A[rep_len(c(TRUE, FALSE), 100), , drop = TRUE])
    expect_error(TST_B[rep_len(c(TRUE, FALSE), 100), , drop = TRUE])
    expect_error(TST_A[rep_len(c(TRUE, FALSE), 100), , drop = FALSE])
    expect_error(TST_B[rep_len(c(TRUE, FALSE), 100), , drop = FALSE])

    expect_error(TST_A[, rep_len(c(TRUE, FALSE), 100)])
    expect_error(TST_B[, rep_len(c(TRUE, FALSE), 100)])
    expect_error(TST_A[, rep_len(c(TRUE, FALSE), 100), drop = TRUE])
    expect_error(TST_B[, rep_len(c(TRUE, FALSE), 100), drop = TRUE])
    expect_error(TST_A[, rep_len(c(TRUE, FALSE), 100), drop = FALSE])
    expect_error(TST_B[, rep_len(c(TRUE, FALSE), 100), drop = FALSE])

    expect_error(TST_A[rep_len(c(TRUE, FALSE), 100), rep_len(c(TRUE, FALSE), 100)])
    expect_error(TST_B[rep_len(c(TRUE, FALSE), 100), rep_len(c(TRUE, FALSE), 100)])
    expect_error(TST_A[rep_len(c(TRUE, FALSE), 100), rep_len(c(TRUE, FALSE), 100), drop = TRUE])
    expect_error(TST_B[rep_len(c(TRUE, FALSE), 100), rep_len(c(TRUE, FALSE), 100), drop = TRUE])
    expect_error(TST_A[rep_len(c(TRUE, FALSE), 100), rep_len(c(TRUE, FALSE), 100), drop = FALSE])
    expect_error(TST_B[rep_len(c(TRUE, FALSE), 100), rep_len(c(TRUE, FALSE), 100), drop = FALSE])

    expect_equal(TST_A[c(FALSE, TRUE), ], TST_B[c(FALSE, TRUE), ])
    expect_equal(TST_A[c(FALSE, TRUE), , drop = TRUE], TST_B[c(FALSE, TRUE), , drop = TRUE])
    expect_equal(TST_A[c(FALSE, TRUE), , drop = FALSE], TST_B[c(FALSE, TRUE), , drop = FALSE])

    expect_equal(TST_A[, c(FALSE, TRUE)], TST_B[, c(FALSE, TRUE)])
    expect_equal(TST_A[, c(FALSE, TRUE), drop = TRUE], TST_B[, c(FALSE, TRUE), drop = TRUE])
    expect_equal(TST_A[, c(FALSE, TRUE), drop = FALSE], TST_B[, c(FALSE, TRUE), drop = FALSE])

    expect_equal(TST_A[c(FALSE, TRUE), c(FALSE, TRUE)], TST_B[c(FALSE, TRUE), c(FALSE, TRUE)])
    expect_equal(TST_A[c(FALSE, TRUE), c(FALSE, TRUE), drop = TRUE], TST_B[c(FALSE, TRUE), c(FALSE, TRUE), drop = TRUE])
    expect_equal(TST_A[c(FALSE, TRUE), c(FALSE, TRUE), drop = FALSE], TST_B[c(FALSE, TRUE), c(FALSE, TRUE), drop = FALSE])

    expect_error(TST_A[rep_len(c(FALSE, TRUE), 100), ])
    expect_error(TST_B[rep_len(c(FALSE, TRUE), 100), ])
    expect_error(TST_A[rep_len(c(FALSE, TRUE), 100), , drop = TRUE])
    expect_error(TST_B[rep_len(c(FALSE, TRUE), 100), , drop = TRUE])
    expect_error(TST_A[rep_len(c(FALSE, TRUE), 100), , drop = FALSE])
    expect_error(TST_B[rep_len(c(FALSE, TRUE), 100), , drop = FALSE])

    expect_error(TST_A[, rep_len(c(FALSE, TRUE), 100)])
    expect_error(TST_B[, rep_len(c(FALSE, TRUE), 100)])
    expect_error(TST_A[, rep_len(c(FALSE, TRUE), 100), drop = TRUE])
    expect_error(TST_B[, rep_len(c(FALSE, TRUE), 100), drop = TRUE])
    expect_error(TST_A[, rep_len(c(FALSE, TRUE), 100), drop = FALSE])
    expect_error(TST_B[, rep_len(c(FALSE, TRUE), 100), drop = FALSE])

    expect_error(TST_A[rep_len(c(FALSE, TRUE), 100), rep_len(c(FALSE, TRUE), 100)])
    expect_error(TST_B[rep_len(c(FALSE, TRUE), 100), rep_len(c(FALSE, TRUE), 100)])
    expect_error(TST_A[rep_len(c(FALSE, TRUE), 100), rep_len(c(FALSE, TRUE), 100), drop = TRUE])
    expect_error(TST_B[rep_len(c(FALSE, TRUE), 100), rep_len(c(FALSE, TRUE), 100), drop = TRUE])
    expect_error(TST_A[rep_len(c(FALSE, TRUE), 100), rep_len(c(FALSE, TRUE), 100), drop = FALSE])
    expect_error(TST_B[rep_len(c(FALSE, TRUE), 100), rep_len(c(FALSE, TRUE), 100), drop = FALSE])

})

test_that("multi indexing by characters works", {

    if (is.null(dimnames(TST_A))) {
        skip("skipping character indexing because dimnames are NULL")
    }

    expect_equal(TST_A["a", ], TST_B["a", ])
    expect_equal(TST_A["a", , drop = TRUE], TST_B["a", , drop = TRUE])
    expect_equal(TST_A["a", , drop = FALSE], TST_B["a", , drop = FALSE])

    expect_equal(TST_A[, "a"], TST_B[, "a"])
    expect_equal(TST_A[, "a", drop = TRUE], TST_B[, "a", drop = TRUE])
    expect_equal(TST_A[, "a", drop = FALSE], TST_B[, "a", drop = FALSE])

    expect_equal(TST_A["a", "a"], TST_B["a", "a"])
    expect_equal(TST_A["a", "a", drop = TRUE], TST_B["a", "a", drop = TRUE])
    expect_equal(TST_A["a", "a", drop = FALSE], TST_B["a", "a", drop = FALSE])

    expect_error(TST_A["x", ])
    expect_error(TST_B["x", ])
    expect_error(TST_A["x", , drop = TRUE])
    expect_error(TST_B["x", , drop = TRUE])
    expect_error(TST_A["x", , drop = FALSE])
    expect_error(TST_B["x", , drop = FALSE])

    expect_error(TST_A[, "x"])
    expect_error(TST_B[, "x"])
    expect_error(TST_A[, "x", drop = TRUE])
    expect_error(TST_B[, "x", drop = TRUE])
    expect_error(TST_A[, "x", drop = FALSE])
    expect_error(TST_B[, "x", drop = FALSE])

    expect_error(TST_A["x", "x"])
    expect_error(TST_B["x", "x"])
    expect_error(TST_A["x", "x", drop = TRUE])
    expect_error(TST_B["x", "x", drop = TRUE])
    expect_error(TST_A["x", "x", drop = FALSE])
    expect_error(TST_B["x", "x", drop = FALSE])

    expect_equal(TST_A[c("a", "b"), ], TST_B[c("a", "b"), ])
    expect_equal(TST_A[c("a", "b"), , drop = TRUE], TST_B[c("a", "b"), , drop = TRUE])
    expect_equal(TST_A[c("a", "b"), , drop = FALSE], TST_B[c("a", "b"), , drop = FALSE])

    expect_equal(TST_A[, c("a", "b")], TST_B[, c("a", "b")])
    expect_equal(TST_A[, c("a", "b"), drop = TRUE], TST_B[, c("a", "b"), drop = TRUE])
    expect_equal(TST_A[, c("a", "b"), drop = FALSE], TST_B[, c("a", "b"), drop = FALSE])

    expect_equal(TST_A[c("a", "b"), c("a", "b")], TST_B[c("a", "b"), c("a", "b")])
    expect_equal(TST_A[c("a", "b"), c("a", "b"), drop = TRUE], TST_B[c("a", "b"), c("a", "b"), drop = TRUE])
    expect_equal(TST_A[c("a", "b"), c("a", "b"), drop = FALSE], TST_B[c("a", "b"), c("a", "b"), drop = FALSE])

    expect_equal(TST_A[c("b", "a"), ], TST_B[c("b", "a"), ])
    expect_equal(TST_A[c("b", "a"), , drop = TRUE], TST_B[c("b", "a"), , drop = TRUE])
    expect_equal(TST_A[c("b", "a"), , drop = FALSE], TST_B[c("b", "a"), , drop = FALSE])

    expect_equal(TST_A[, c("b", "a")], TST_B[, c("b", "a")])
    expect_equal(TST_A[, c("b", "a"), drop = TRUE], TST_B[, c("b", "a"), drop = TRUE])
    expect_equal(TST_A[, c("b", "a"), drop = FALSE], TST_B[, c("b", "a"), drop = FALSE])

    expect_equal(TST_A[c("b", "a"), c("b", "a")], TST_B[c("b", "a"), c("b", "a")])
    expect_equal(TST_A[c("b", "a"), c("b", "a"), drop = TRUE], TST_B[c("b", "a"), c("b", "a"), drop = TRUE])
    expect_equal(TST_A[c("b", "a"), c("b", "a"), drop = FALSE], TST_B[c("b", "a"), c("b", "a"), drop = FALSE])

    expect_error(TST_A[c("a", "x"), ])
    expect_error(TST_B[c("a", "x"), ])
    expect_error(TST_A[c("a", "x"), , drop = TRUE])
    expect_error(TST_B[c("a", "x"), , drop = TRUE])
    expect_error(TST_A[c("a", "x"), , drop = FALSE])
    expect_error(TST_B[c("a", "x"), , drop = FALSE])

    expect_error(TST_A[, c("a", "x")])
    expect_error(TST_B[, c("a", "x")])
    expect_error(TST_A[, c("a", "x"), drop = TRUE])
    expect_error(TST_B[, c("a", "x"), drop = TRUE])
    expect_error(TST_A[, c("a", "x"), drop = FALSE])
    expect_error(TST_B[, c("a", "x"), drop = FALSE])

    expect_error(TST_A[c("a", "x"), c("a", "x")])
    expect_error(TST_B[c("a", "x"), c("a", "x")])
    expect_error(TST_A[c("a", "x"), c("a", "x"), drop = TRUE])
    expect_error(TST_B[c("a", "x"), c("a", "x"), drop = TRUE])
    expect_error(TST_A[c("a", "x"), c("a", "x"), drop = FALSE])
    expect_error(TST_B[c("a", "x"), c("a", "x"), drop = FALSE])

    expect_error(TST_A[c("x", "a"), ])
    expect_error(TST_B[c("x", "a"), ])
    expect_error(TST_A[c("x", "a"), , drop = TRUE])
    expect_error(TST_B[c("x", "a"), , drop = TRUE])
    expect_error(TST_A[c("x", "a"), , drop = FALSE])
    expect_error(TST_B[c("x", "a"), , drop = FALSE])

    expect_error(TST_A[, c("x", "a")])
    expect_error(TST_B[, c("x", "a")])
    expect_error(TST_A[, c("x", "a"), drop = TRUE])
    expect_error(TST_B[, c("x", "a"), drop = TRUE])
    expect_error(TST_A[, c("x", "a"), drop = FALSE])
    expect_error(TST_B[, c("x", "a"), drop = FALSE])

    expect_error(TST_A[c("x", "a"), c("x", "a")])
    expect_error(TST_B[c("x", "a"), c("x", "a")])
    expect_error(TST_A[c("x", "a"), c("x", "a"), drop = TRUE])
    expect_error(TST_B[c("x", "a"), c("x", "a"), drop = TRUE])
    expect_error(TST_A[c("x", "a"), c("x", "a"), drop = FALSE])
    expect_error(TST_B[c("x", "a"), c("x", "a"), drop = FALSE])

})

test_that("multi indexing by NA works", {

    expect_equal(TST_A[c(1, NA), ], TST_B[c(1, NA), ])
    expect_equal(TST_A[c(1, NA), , drop = TRUE], TST_B[c(1, NA), , drop = TRUE])
    expect_equal(TST_A[c(1, NA), , drop = FALSE], TST_B[c(1, NA), , drop = FALSE])

    expect_equal(TST_A[, c(1, NA)], TST_B[, c(1, NA)])
    expect_equal(TST_A[, c(1, NA), drop = TRUE], TST_B[, c(1, NA), drop = TRUE])
    expect_equal(TST_A[, c(1, NA), drop = FALSE], TST_B[, c(1, NA), drop = FALSE])

    expect_equal(TST_A[c(1, NA), c(1, NA)], TST_B[c(1, NA), c(1, NA)])
    expect_equal(TST_A[c(1, NA), c(1, NA), drop = TRUE], TST_B[c(1, NA), c(1, NA), drop = TRUE])
    expect_equal(TST_A[c(1, NA), c(1, NA), drop = FALSE], TST_B[c(1, NA), c(1, NA), drop = FALSE])

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

    expect_error(TST_A[c("a", NA), ])
    expect_error(TST_B[c("a", NA), ])
    expect_error(TST_A[c("a", NA), , drop = TRUE])
    expect_error(TST_B[c("a", NA), , drop = TRUE])
    expect_error(TST_A[c("a", NA), , drop = FALSE])
    expect_error(TST_B[c("a", NA), , drop = FALSE])

    expect_error(TST_A[, c("a", NA)])
    expect_error(TST_B[, c("a", NA)])
    expect_error(TST_A[, c("a", NA), drop = TRUE])
    expect_error(TST_B[, c("a", NA), drop = TRUE])
    expect_error(TST_A[, c("a", NA), drop = FALSE])
    expect_error(TST_B[, c("a", NA), drop = FALSE])

    expect_error(TST_A[c("a", NA), c("a", NA)])
    expect_error(TST_B[c("a", NA), c("a", NA)])
    expect_error(TST_A[c("a", NA), c("a", NA), drop = TRUE])
    expect_error(TST_B[c("a", NA), c("a", NA), drop = TRUE])
    expect_error(TST_A[c("a", NA), c("a", NA), drop = FALSE])
    expect_error(TST_B[c("a", NA), c("a", NA), drop = FALSE])

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
