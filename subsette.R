b <- matrix(data = rnorm(25), nrow = 5, ncol = 5)
dimnames(b) <- list(letters[1:5], letters[1:5])

a <- structure(list(), class = "TestMatrix")

dim.TestMatrix <- function(x) c(5, 5)

dimnames.TestMatrix <- function(x) dimnames(b)

vectorSubset <- function(x, i) {
    # Dispatch to b instead to x for this demo
    b[i, drop = FALSE]
}

matrixSubset <- function(x, i, j) {
    # Dispatch to b instead to x for this demo
    b[i, j, drop = FALSE]
}

subsette <- function(x, i, j, drop = TRUE) {

    # Convert non-numeric types to positive integers
    convertIndex <- function(x, i, type) {
        if (type == "k") {
            # Single Index
            n <- nrow(x) * ncol(x)
            checkBounds <- FALSE
        } else if (type == "i") {
            # Rows of Multi Index
            n <- nrow(x)
            checkBounds <- TRUE
        } else if (type == "j") {
            # Columns of Multi Index
            n <- ncol(x)
            checkBounds <- TRUE
        }
        if (typeof(i) == "logical") { # x[TRUE]
            len_initial_i <- length(i)
            if (type != "k" && len_initial_i > n) {
                stop("(subscript) logical subscript too long")
            }
            len_initial_i_sans_false <- len_initial_i - sum(i == FALSE, na.rm = T)
            # Expand logical index to length of vector while preserving
            # positions of missing values
            i <- rep_len(i, n)
            pos_true <- which(i)
            pos_na <- which(is.na(i))
            i <- sort(c(pos_true, pos_na))
            i[i %in% pos_na] <- NA
            # Expand logical index to length of i for single indices
            if (len_initial_i_sans_false > length(i)) {
                i <- i[1:len_initial_i_sans_false]
            }
        } else if (typeof(i) == "character") { # x["a"]
            if (type == "k") {
                if (class(i) == "matrix" && ncol(i) == 2L) {
                    i <- (match(i[, 2], colnames(x)) - 1) * nrow(x) + match(i[, 1], rownames(x))
                    if (any(is.na(i))) {
                        stop("subscript out of bounds")
                    }
                } else {
                    i[] <- NA
                }
            } else {
                if (type == "i") {
                    names <- rownames(x)
                } else {
                    names <- colnames(x)
                }
                i <- match(i, names, nomatch = n + 1L) # intentionally overstep bound
            }
        } else if (type == "k" && class(i) == "matrix" && ncol(i) == 2L && is.numeric(i)) { # x[y > 1]
            i <- (as.integer(i[, 2]) - 1) * nrow(x) + as.integer(i[, 1])
            checkBounds <- TRUE
        } else if (is.numeric(i)) { # x[1], x[-1], x[0]
            # Remove all 0s
            i <- i[i != 0]
            if (length(i) == 0L) { # x[0]
                stop("Unsupported index type")
            } else {
                i_is_na <- is.na(i)
                # Negative integers are not allowed to be combined with
                # positive integers or missing values
                if (any(i < 0, na.rm = TRUE) && (any(i > 0, na.rm = TRUE) || any(i_is_na))) {
                    stop("only 0's may be mixed with negative subscripts")
                } else if (all(i < 0, na.rm = TRUE)) { # x[-1]
                    i <- seq(1, n)[i]
                }
            }
        }
        if (checkBounds && any(i > n, na.rm = TRUE)) {
            stop("subscript out of bounds")
        }
        return(i)
    }

    nargs <- nargs()
    # Subtract optional argument drop from nargs if explicitly passed (I still
    # don't fully understand how this works: x[] and x[drop = TRUE] both result
    # in nargs == 2L)
    if (!missing(drop)) {
        nargs <- nargs - 1L
    }

    # Single Index: x[i]
    if (nargs == 2L && !missing(i) && missing(j)) {
        i <- convertIndex(x, i, "k")
        subset <- vectorSubset(x, i)
    # Multi Index: x[i, j], x[i, ], or x[, j]
    } else if (nargs == 3L && (!missing(i) || !missing(j))) {
        if (missing(i)) {
            i <- seq(1, nrow(x))
        } else {
            i <- convertIndex(x, i, "i")
        }
        if (missing(j)) {
            j <- seq(1, ncol(x))
        } else {
            j <- convertIndex(x, j, "j")
        }
        subset <- matrixSubset(x, i, j)
        # Let R handle drop behavior
        if (drop == TRUE && (nrow(subset) == 1L || ncol(subset) == 1L)) {
            subset <- subset[, ]
        }
    # No Index: x[] or x[, ]
    } else {
        i <- seq(1, nrow(x))
        j <- seq(1, ncol(x))
        subset <- matrixSubset(x, i, j)
    }

    return(subset)

}

`[.TestMatrix` <- subsette


library(testthat)

set.seed(4711)

test_that("single indexing by nothing works", {

    expect_equal(a[], b[])
    expect_equal(a[drop = TRUE], b[drop = TRUE])
    expect_equal(a[drop = FALSE], b[drop = FALSE])
})

test_that("single indexing by positive integers works", {

    expect_equal(a[1], b[1])
    expect_equal(a[1, drop = TRUE], b[1, drop = TRUE])
    expect_equal(a[1, drop = FALSE], b[1, drop = FALSE])

    expect_equal(a[c(1, 2)], b[c(1, 2)])
    expect_equal(a[c(1, 2), drop = TRUE], b[c(1, 2), drop = TRUE])
    expect_equal(a[c(1, 2), drop = FALSE], b[c(1, 2), drop = FALSE])

    expect_equal(a[c(2, 1)], b[c(2, 1)])
    expect_equal(a[c(2, 1), drop = TRUE], b[c(2, 1), drop = TRUE])
    expect_equal(a[c(2, 1), drop = FALSE], b[c(2, 1), drop = FALSE])

    expect_equal(a[1.1], b[1.1])
    expect_equal(a[1.1, drop = TRUE], b[1.1, drop = TRUE])
    expect_equal(a[1.1, drop = FALSE], b[1.1, drop = FALSE])

    expect_equal(a[c(1.1, 2.1)], b[c(1.1, 2.1)])
    expect_equal(a[c(1.1, 2.1), drop = TRUE], b[c(1.1, 2.1), drop = TRUE])
    expect_equal(a[c(1.1, 2.1), drop = FALSE], b[c(1.1, 2.1), drop = FALSE])

    expect_equal(a[c(2.1, 1.1)], b[c(2.1, 1.1)])
    expect_equal(a[c(2.1, 1.1), drop = TRUE], b[c(2.1, 1.1), drop = TRUE])
    expect_equal(a[c(2.1, 1.1), drop = FALSE], b[c(2.1, 1.1), drop = FALSE])

    expect_equal(a[1.9], b[1.9])
    expect_equal(a[1.9, drop = TRUE], b[1.9, drop = TRUE])
    expect_equal(a[1.9, drop = FALSE], b[1.9, drop = FALSE])

    expect_equal(a[c(1.9, 2.9)], b[c(1.9, 2.9)])
    expect_equal(a[c(1.9, 2.9), drop = TRUE], b[c(1.9, 2.9), drop = TRUE])
    expect_equal(a[c(1.9, 2.9), drop = FALSE], b[c(1.9, 2.9), drop = FALSE])

    expect_equal(a[c(2.9, 1.9)], b[c(2.9, 1.9)])
    expect_equal(a[c(2.9, 1.9), drop = TRUE], b[c(2.9, 1.9), drop = TRUE])
    expect_equal(a[c(2.9, 1.9), drop = FALSE], b[c(2.9, 1.9), drop = FALSE])

    expect_equal(a[100], b[100])
    expect_equal(a[100, drop = TRUE], b[100, drop = TRUE])
    expect_equal(a[100, drop = FALSE], b[100, drop = FALSE])

    expect_equal(a[c(100, 2)], b[c(100, 2)])
    expect_equal(a[c(100, 2), drop = TRUE], b[c(100, 2), drop = TRUE])
    expect_equal(a[c(100, 2), drop = FALSE], b[c(100, 2), drop = FALSE])

    expect_equal(a[c(2, 100)], b[c(2, 100)])
    expect_equal(a[c(2, 100), drop = TRUE], b[c(2, 100), drop = TRUE])
    expect_equal(a[c(2, 100), drop = FALSE], b[c(2, 100), drop = FALSE])

})

test_that("single indexing by negative integers works", {

    expect_equal(a[-1], b[-1])
    expect_equal(a[-1, drop = TRUE], b[-1, drop = TRUE])
    expect_equal(a[-1, drop = FALSE], b[-1, drop = FALSE])

    expect_equal(a[c(-1, -2)], b[c(-1, -2)])
    expect_equal(a[c(-1, -2), drop = TRUE], b[c(-1, -2), drop = TRUE])
    expect_equal(a[c(-1, -2), drop = FALSE], b[c(-1, -2), drop = FALSE])

    expect_equal(a[c(-2, -1)], b[c(-2, -1)])
    expect_equal(a[c(-2, -1), drop = TRUE], b[c(-2, -1), drop = TRUE])
    expect_equal(a[c(-2, -1), drop = FALSE], b[c(-2, -1), drop = FALSE])

    expect_equal(a[-1.1], b[-1.1])
    expect_equal(a[-1.1, drop = TRUE], b[-1.1, drop = TRUE])
    expect_equal(a[-1.1, drop = FALSE], b[-1.1, drop = FALSE])

    expect_equal(a[c(-1.1, -2.1)], b[c(-1.1, -2.1)])
    expect_equal(a[c(-1.1, -2.1), drop = TRUE], b[c(-1.1, -2.1), drop = TRUE])
    expect_equal(a[c(-1.1, -2.1), drop = FALSE], b[c(-1.1, -2.1), drop = FALSE])

    expect_equal(a[c(-2.1, -1.1)], b[c(-2.1, -1.1)])
    expect_equal(a[c(-2.1, -1.1), drop = TRUE], b[c(-2.1, -1.1), drop = TRUE])
    expect_equal(a[c(-2.1, -1.1), drop = FALSE], b[c(-2.1, -1.1), drop = FALSE])

    expect_equal(a[-1.9], b[-1.9])
    expect_equal(a[-1.9, drop = TRUE], b[-1.9, drop = TRUE])
    expect_equal(a[-1.9, drop = FALSE], b[-1.9, drop = FALSE])

    expect_equal(a[c(-1.9, -2.9)], b[c(-1.9, -2.9)])
    expect_equal(a[c(-1.9, -2.9), drop = TRUE], b[c(-1.9, -2.9), drop = TRUE])
    expect_equal(a[c(-1.9, -2.9), drop = FALSE], b[c(-1.9, -2.9), drop = FALSE])

    expect_equal(a[c(-2.9, -1.9)], b[c(-2.9, -1.9)])
    expect_equal(a[c(-2.9, -1.9), drop = TRUE], b[c(-2.9, -1.9), drop = TRUE])
    expect_equal(a[c(-2.9, -1.9), drop = FALSE], b[c(-2.9, -1.9), drop = FALSE])

    expect_equal(a[-100], b[-100])
    expect_equal(a[-100, drop = TRUE], b[-100, drop = TRUE])
    expect_equal(a[-100, drop = FALSE], b[-100, drop = FALSE])

    expect_equal(a[c(-100, -2)], b[c(-100, -2)])
    expect_equal(a[c(-100, -2), drop = TRUE], b[c(-100, -2), drop = TRUE])
    expect_equal(a[c(-100, -2), drop = FALSE], b[c(-100, -2), drop = FALSE])

    expect_equal(a[c(-2, -100)], b[c(-2, -100)])
    expect_equal(a[c(-2, -100), drop = TRUE], b[c(-2, -100), drop = TRUE])
    expect_equal(a[c(-2, -100), drop = FALSE], b[c(-2, -100), drop = FALSE])

})

test_that("single indexing by logicals works", {

    expect_equal(a[TRUE], b[TRUE])
    expect_equal(a[TRUE, drop = TRUE], b[TRUE, drop = TRUE])
    expect_equal(a[TRUE, drop = FALSE], b[TRUE, drop = FALSE])

    expect_equal(a[FALSE], b[FALSE])
    expect_equal(a[FALSE, drop = FALSE], b[FALSE, drop = FALSE])
    expect_equal(a[FALSE, drop = FALSE], b[FALSE, drop = FALSE])

    expect_equal(a[c(TRUE, FALSE)], b[c(TRUE, FALSE)])
    expect_equal(a[c(TRUE, FALSE), drop = TRUE], b[c(TRUE, FALSE), drop = TRUE])
    expect_equal(a[c(TRUE, FALSE), drop = FALSE], b[c(TRUE, FALSE), drop = FALSE])

    expect_equal(a[c(FALSE, TRUE)], b[c(FALSE, TRUE)])
    expect_equal(a[c(FALSE, TRUE), drop = TRUE], b[c(FALSE, TRUE), drop = TRUE])
    expect_equal(a[c(FALSE, TRUE), drop = FALSE], b[c(FALSE, TRUE), drop = FALSE])

    expect_equal(a[rep_len(TRUE, 100)], b[rep_len(TRUE, 100)])
    expect_equal(a[rep_len(TRUE, 100), drop = rep_len(TRUE, 100)], b[rep_len(TRUE, 100), drop = rep_len(TRUE, 100)])
    expect_equal(a[rep_len(TRUE, 100), drop = FALSE], b[rep_len(TRUE, 100), drop = FALSE])

    expect_equal(a[rep_len(FALSE, 100)], b[rep_len(FALSE, 100)])
    expect_equal(a[rep_len(FALSE, 100), drop = rep_len(FALSE, 100)], b[rep_len(FALSE, 100), drop = rep_len(FALSE, 100)])
    expect_equal(a[rep_len(FALSE, 100), drop = rep_len(FALSE, 100)], b[rep_len(FALSE, 100), drop = rep_len(FALSE, 100)])

    expect_equal(a[rep_len(c(TRUE, FALSE), 100)], b[rep_len(c(TRUE, FALSE), 100)])
    expect_equal(a[rep_len(c(TRUE, FALSE), 100), drop = TRUE], b[rep_len(c(TRUE, FALSE), 100), drop = TRUE])
    expect_equal(a[rep_len(c(TRUE, FALSE), 100), drop = FALSE], b[rep_len(c(TRUE, FALSE), 100), drop = FALSE])

    expect_equal(a[rep_len(c(FALSE, TRUE), 100)], b[rep_len(c(FALSE, TRUE), 100)])
    expect_equal(a[rep_len(c(FALSE, TRUE), 100), drop = TRUE], b[rep_len(c(FALSE, TRUE), 100), drop = TRUE])
    expect_equal(a[rep_len(c(FALSE, TRUE), 100), drop = FALSE], b[rep_len(c(FALSE, TRUE), 100), drop = FALSE])

})

test_that("single indexing by characters works", {

    expect_equal(a["a"], b["a"])
    expect_equal(a["a", drop = TRUE], b["a", drop = TRUE])
    expect_equal(a["a", drop = FALSE], b["a", drop = FALSE])

    expect_equal(a["x"], b["x"])
    expect_equal(a["x", drop = TRUE], b["x", drop = TRUE])
    expect_equal(a["x", drop = FALSE], b["x", drop = FALSE])

})

test_that("single indexing by NA works", {

    expect_equal(a[c(1, NA)], b[c(1, NA)])
    expect_equal(a[c(1, NA), drop = TRUE], b[c(1, NA), drop = TRUE])
    expect_equal(a[c(1, NA), drop = FALSE], b[c(1, NA), drop = FALSE])

    expect_error(a[c(-1, NA)])
    expect_error(b[c(-1, NA)])
    expect_error(a[c(-1, NA), drop = TRUE])
    expect_error(b[c(-1, NA), drop = TRUE])
    expect_error(a[c(-1, NA), drop = FALSE])
    expect_error(b[c(-1, NA), drop = FALSE])

    expect_equal(a[c("a", NA)], b[c("a", NA)])
    expect_equal(a[c("a", NA), drop = TRUE], b[c("a", NA), drop = TRUE])
    expect_equal(a[c("a", NA), drop = FALSE], b[c("a", NA), drop = FALSE])

    expect_equal(a[c(TRUE, NA)], b[c(TRUE, NA)])
    expect_equal(a[c(TRUE, NA), drop = TRUE], b[c(TRUE, NA), drop = TRUE])
    expect_equal(a[c(TRUE, NA), drop = FALSE], b[c(TRUE, NA), drop = FALSE])

    expect_equal(a[c(FALSE, NA)], b[c(FALSE, NA)])
    expect_equal(a[c(FALSE, NA), drop = TRUE], b[c(FALSE, NA), drop = TRUE])
    expect_equal(a[c(FALSE, NA), drop = FALSE], b[c(FALSE, NA), drop = FALSE])

    expect_equal(a[NA], b[NA])
    expect_equal(a[NA, drop = TRUE], b[NA, drop = TRUE])
    expect_equal(a[NA, drop = FALSE], b[NA, drop = FALSE])

    expect_equal(a[NA_integer_], b[NA_integer_])
    expect_equal(a[NA_integer_, drop = TRUE], b[NA_integer_, drop = TRUE])
    expect_equal(a[NA_integer_, drop = FALSE], b[NA_integer_, drop = FALSE])

    expect_equal(a[NA_character_], b[NA_character_])
    expect_equal(a[NA_character_, drop = TRUE], b[NA_character_, drop = TRUE])
    expect_equal(a[NA_character_, drop = FALSE], b[NA_character_, drop = FALSE])

})

test_that("single indexing by zero works", {

    expect_error(a[0]) # Not implemented
    expect_error(a[0, drop = TRUE]) # Not implemented
    expect_error(a[0, drop = FALSE]) # Not implemented

    expect_equal(a[c(0, 1)], b[c(0, 1)])
    expect_equal(a[c(0, 1), drop = TRUE], b[c(0, 1), drop = TRUE])
    expect_equal(a[c(0, 1), drop = FALSE], b[c(0, 1), drop = FALSE])

    expect_equal(a[c(0, -1)], b[c(0, -1)])
    expect_equal(a[c(0, -1), drop = TRUE], b[c(0, -1), drop = TRUE])
    expect_equal(a[c(0, -1), drop = FALSE], b[c(0, -1), drop = FALSE])

    expect_error(a[c(0, 1, -1)])
    expect_error(b[c(0, 1, -1)])
    expect_error(a[c(0, 1, -1), drop = TRUE])
    expect_error(b[c(0, 1, -1), drop = TRUE])
    expect_error(a[c(0, 1, -1), drop = FALSE])
    expect_error(b[c(0, 1, -1), drop = FALSE])

})

test_that("single indexing by matrix works", {

    m <- matrix(data = rnorm(25), nrow = 5, ncol = 5)
    expect_equal(a[m > 1], b[m > 1])
    expect_equal(a[m > 1, drop = TRUE], b[m > 1, drop = TRUE])
    expect_equal(a[m > 1, drop = FALSE], b[m > 1, drop = FALSE])

    m <- matrix(data = c(1, 1, 2, 2), ncol = 2, byrow = TRUE)
    expect_equal(a[m], b[m])
    expect_equal(a[m, drop = TRUE], b[m, drop = TRUE])
    expect_equal(a[m, drop = FALSE], b[m, drop = FALSE])

    m <- matrix(data = c(1, 1, 2, 2), ncol = 2, byrow = TRUE)
    expect_equal(a[m], b[m])
    expect_equal(a[m, drop = TRUE], b[m, drop = TRUE])
    expect_equal(a[m, drop = FALSE], b[m, drop = FALSE])

    m <- matrix(data = c(2, 2, 1, 1), ncol = 2, byrow = TRUE)
    expect_equal(a[m], b[m])
    expect_equal(a[m, drop = TRUE], b[m, drop = TRUE])
    expect_equal(a[m, drop = FALSE], b[m, drop = FALSE])

    m <- matrix(data = c(1.1, 1.1, 2.1, 2.1), ncol = 2, byrow = TRUE)
    expect_equal(a[m], b[m])
    expect_equal(a[m, drop = TRUE], b[m, drop = TRUE])
    expect_equal(a[m, drop = FALSE], b[m, drop = FALSE])

    m <- matrix(data = c(2.1, 2.1, 1.1, 1.1), ncol = 2, byrow = TRUE)
    expect_equal(a[m], b[m])
    expect_equal(a[m, drop = TRUE], b[m, drop = TRUE])
    expect_equal(a[m, drop = FALSE], b[m, drop = FALSE])

    m <- matrix(data = c(1.9, 1.9, 2.9, 2.9), ncol = 2, byrow = TRUE)
    expect_equal(a[m], b[m])
    expect_equal(a[m, drop = TRUE], b[m, drop = TRUE])
    expect_equal(a[m, drop = FALSE], b[m, drop = FALSE])

    m <- matrix(data = c(2.9, 2.9, 1.9, 1.9), ncol = 2, byrow = TRUE)
    expect_equal(a[m], b[m])
    expect_equal(a[m, drop = TRUE], b[m, drop = TRUE])
    expect_equal(a[m, drop = FALSE], b[m, drop = FALSE])

    m <- matrix(data = c(100, 100), ncol = 2, byrow = TRUE)
    expect_error(a[m])
    expect_error(b[m])
    expect_error(a[m, drop = TRUE])
    expect_error(b[m, drop = TRUE])
    expect_error(a[m, drop = FALSE])
    expect_error(b[m, drop = FALSE])

    m <- matrix(data = c("a", "a", "b", "b"), ncol = 2, byrow = TRUE)
    expect_equal(a[m], b[m])
    expect_equal(a[m, drop = TRUE], b[m, drop = TRUE])
    expect_equal(a[m, drop = FALSE], b[m, drop = FALSE])

    m <- matrix(data = c("b", "b", "a", "a"), ncol = 2, byrow = TRUE)
    expect_equal(a[m], b[m])
    expect_equal(a[m, drop = TRUE], b[m, drop = TRUE])
    expect_equal(a[m, drop = FALSE], b[m, drop = FALSE])

    m <- matrix(data = c("x", "x"), ncol = 2, byrow = TRUE)
    expect_error(a[m])
    expect_error(b[m])
    expect_error(a[m, drop = TRUE])
    expect_error(b[m, drop = TRUE])
    expect_error(a[m, drop = FALSE])
    expect_error(b[m, drop = FALSE])

})

test_that("multi indexing by nothing works", {

    expect_equal(a[, ], b[, ])
    expect_equal(a[, , drop = TRUE], b[, , drop = TRUE])
    expect_equal(a[, , drop = FALSE], b[, , drop = FALSE])

})

test_that("multi indexing by positive integers works", {

    expect_equal(a[1, ], b[1, ])
    expect_equal(a[1, , drop = TRUE], b[1, , drop = TRUE])
    expect_equal(a[1, , drop = FALSE], b[1, , drop = FALSE])

    expect_equal(a[, 1], b[, 1])
    expect_equal(a[, 1, drop = TRUE], b[, 1, drop = TRUE])
    expect_equal(a[, 1, drop = FALSE], b[, 1, drop = FALSE])

    expect_equal(a[1, 1], b[1, 1])
    expect_equal(a[1, 1, drop = TRUE], b[1, 1, drop = TRUE])
    expect_equal(a[1, 1, drop = FALSE], b[1, 1, drop = FALSE])

    expect_equal(a[c(1, 2), ], b[c(1, 2), ])
    expect_equal(a[c(1, 2), , drop = TRUE], b[c(1, 2), , drop = TRUE])
    expect_equal(a[c(1, 2), , drop = FALSE], b[c(1, 2), , drop = FALSE])

    expect_equal(a[, c(1, 2)], b[, c(1, 2)])
    expect_equal(a[, c(1, 2), drop = TRUE], b[, c(1, 2), drop = TRUE])
    expect_equal(a[, c(1, 2), drop = FALSE], b[, c(1, 2), drop = FALSE])

    expect_equal(a[c(1, 2), c(1, 2)], b[c(1, 2), c(1, 2)])
    expect_equal(a[c(1, 2), c(1, 2), drop = TRUE], b[c(1, 2), c(1, 2), drop = TRUE])
    expect_equal(a[c(1, 2), c(1, 2), drop = FALSE], b[c(1, 2), c(1, 2), drop = FALSE])

    expect_equal(a[c(2, 1), ], b[c(2, 1), ])
    expect_equal(a[c(2, 1), , drop = TRUE], b[c(2, 1), , drop = TRUE])
    expect_equal(a[c(2, 1), , drop = FALSE], b[c(2, 1), , drop = FALSE])

    expect_equal(a[, c(2, 1)], b[, c(2, 1)])
    expect_equal(a[, c(2, 1), drop = TRUE], b[, c(2, 1), drop = TRUE])
    expect_equal(a[, c(2, 1), drop = FALSE], b[, c(2, 1), drop = FALSE])

    expect_equal(a[c(2, 1), c(2, 1)], b[c(2, 1), c(2, 1)])
    expect_equal(a[c(2, 1), c(2, 1), drop = TRUE], b[c(2, 1), c(2, 1), drop = TRUE])
    expect_equal(a[c(2, 1), c(2, 1), drop = FALSE], b[c(2, 1), c(2, 1), drop = FALSE])

    expect_equal(a[1.1, ], b[1.1, ])
    expect_equal(a[1.1, , drop = TRUE], b[1.1, , drop = TRUE])
    expect_equal(a[1.1, , drop = FALSE], b[1.1, , drop = FALSE])

    expect_equal(a[, 1.1], b[, 1.1])
    expect_equal(a[, 1.1, drop = TRUE], b[, 1.1, drop = TRUE])
    expect_equal(a[, 1.1, drop = FALSE], b[, 1.1, drop = FALSE])

    expect_equal(a[1.1, 1.1], b[1.1, 1.1])
    expect_equal(a[1.1, 1.1, drop = TRUE], b[1.1, 1.1, drop = TRUE])
    expect_equal(a[1.1, 1.1, drop = FALSE], b[1.1, 1.1, drop = FALSE])

    expect_equal(a[c(1.1, 2.1), ], b[c(1.1, 2.1), ])
    expect_equal(a[c(1.1, 2.1), , drop = TRUE], b[c(1.1, 2.1), , drop = TRUE])
    expect_equal(a[c(1.1, 2.1), , drop = FALSE], b[c(1.1, 2.1), , drop = FALSE])

    expect_equal(a[, c(1.1, 2.1)], b[, c(1.1, 2.1)])
    expect_equal(a[, c(1.1, 2.1), drop = TRUE], b[, c(1.1, 2.1), drop = TRUE])
    expect_equal(a[, c(1.1, 2.1), drop = FALSE], b[, c(1.1, 2.1), drop = FALSE])

    expect_equal(a[c(1.1, 2.1), c(1.1, 2.1)], b[c(1.1, 2.1), c(1.1, 2.1)])
    expect_equal(a[c(1.1, 2.1), c(1.1, 2.1), drop = TRUE], b[c(1.1, 2.1), c(1.1, 2.1), drop = TRUE])
    expect_equal(a[c(1.1, 2.1), c(1.1, 2.1), drop = FALSE], b[c(1.1, 2.1), c(1.1, 2.1), drop = FALSE])

    expect_equal(a[c(2.1, 1.1), ], b[c(2.1, 1.1), ])
    expect_equal(a[c(2.1, 1.1), , drop = TRUE], b[c(2.1, 1.1), , drop = TRUE])
    expect_equal(a[c(2.1, 1.1), , drop = FALSE], b[c(2.1, 1.1), , drop = FALSE])

    expect_equal(a[, c(2.1, 1.1)], b[, c(2.1, 1.1)])
    expect_equal(a[, c(2.1, 1.1), drop = TRUE], b[, c(2.1, 1.1), drop = TRUE])
    expect_equal(a[, c(2.1, 1.1), drop = FALSE], b[, c(2.1, 1.1), drop = FALSE])

    expect_equal(a[c(2.1, 1.1), c(2.1, 1.1)], b[c(2.1, 1.1), c(2.1, 1.1)])
    expect_equal(a[c(2.1, 1.1), c(2.1, 1.1), drop = TRUE], b[c(2.1, 1.1), c(2.1, 1.1), drop = TRUE])
    expect_equal(a[c(2.1, 1.1), c(2.1, 1.1), drop = FALSE], b[c(2.1, 1.1), c(2.1, 1.1), drop = FALSE])

    expect_equal(a[1.9, ], b[1.9, ])
    expect_equal(a[1.9, , drop = TRUE], b[1.9, , drop = TRUE])
    expect_equal(a[1.9, , drop = FALSE], b[1.9, , drop = FALSE])

    expect_equal(a[, 1.9], b[, 1.9])
    expect_equal(a[, 1.9, drop = TRUE], b[, 1.9, drop = TRUE])
    expect_equal(a[, 1.9, drop = FALSE], b[, 1.9, drop = FALSE])

    expect_equal(a[1.9, 1.9], b[1.9, 1.9])
    expect_equal(a[1.9, 1.9, drop = TRUE], b[1.9, 1.9, drop = TRUE])
    expect_equal(a[1.9, 1.9, drop = FALSE], b[1.9, 1.9, drop = FALSE])

    expect_equal(a[c(1.9, 2.9), ], b[c(1.9, 2.9), ])
    expect_equal(a[c(1.9, 2.9), , drop = TRUE], b[c(1.9, 2.9), , drop = TRUE])
    expect_equal(a[c(1.9, 2.9), , drop = FALSE], b[c(1.9, 2.9), , drop = FALSE])

    expect_equal(a[, c(1.9, 2.9)], b[, c(1.9, 2.9)])
    expect_equal(a[, c(1.9, 2.9), drop = TRUE], b[, c(1.9, 2.9), drop = TRUE])
    expect_equal(a[, c(1.9, 2.9), drop = FALSE], b[, c(1.9, 2.9), drop = FALSE])

    expect_equal(a[c(1.9, 2.9), c(1.9, 2.9)], b[c(1.9, 2.9), c(1.9, 2.9)])
    expect_equal(a[c(1.9, 2.9), c(1.9, 2.9), drop = TRUE], b[c(1.9, 2.9), c(1.9, 2.9), drop = TRUE])
    expect_equal(a[c(1.9, 2.9), c(1.9, 2.9), drop = FALSE], b[c(1.9, 2.9), c(1.9, 2.9), drop = FALSE])

    expect_equal(a[c(2.9, 1.9), ], b[c(2.9, 1.9), ])
    expect_equal(a[c(2.9, 1.9), , drop = TRUE], b[c(2.9, 1.9), , drop = TRUE])
    expect_equal(a[c(2.9, 1.9), , drop = FALSE], b[c(2.9, 1.9), , drop = FALSE])

    expect_equal(a[, c(2.9, 1.9)], b[, c(2.9, 1.9)])
    expect_equal(a[, c(2.9, 1.9), drop = TRUE], b[, c(2.9, 1.9), drop = TRUE])
    expect_equal(a[, c(2.9, 1.9), drop = FALSE], b[, c(2.9, 1.9), drop = FALSE])

    expect_equal(a[c(2.9, 1.9), c(2.9, 1.9)], b[c(2.9, 1.9), c(2.9, 1.9)])
    expect_equal(a[c(2.9, 1.9), c(2.9, 1.9), drop = TRUE], b[c(2.9, 1.9), c(2.9, 1.9), drop = TRUE])
    expect_equal(a[c(2.9, 1.9), c(2.9, 1.9), drop = FALSE], b[c(2.9, 1.9), c(2.9, 1.9), drop = FALSE])

    expect_error(a[100, ])
    expect_error(b[100, ])
    expect_error(a[100, , drop = TRUE])
    expect_error(b[100, , drop = TRUE])
    expect_error(a[100, , drop = FALSE])
    expect_error(b[100, , drop = FALSE])

    expect_error(a[, 100])
    expect_error(b[, 100])
    expect_error(a[, 100, drop = TRUE])
    expect_error(b[, 100, drop = TRUE])
    expect_error(a[, 100, drop = FALSE])
    expect_error(b[, 100, drop = FALSE])

    expect_error(a[100, 100])
    expect_error(b[100, 100])
    expect_error(a[100, 100, drop = TRUE])
    expect_error(b[100, 100, drop = TRUE])
    expect_error(a[100, 100, drop = FALSE])
    expect_error(b[100, 100, drop = FALSE])

    expect_error(a[c(100, 2), ])
    expect_error(b[c(100, 2), ])
    expect_error(a[c(100, 2), , drop = TRUE])
    expect_error(b[c(100, 2), , drop = TRUE])
    expect_error(a[c(100, 2), , drop = FALSE])
    expect_error(b[c(100, 2), , drop = FALSE])

    expect_error(a[, c(100, 2)])
    expect_error(b[, c(100, 2)])
    expect_error(a[, c(100, 2), drop = TRUE])
    expect_error(b[, c(100, 2), drop = TRUE])
    expect_error(a[, c(100, 2), drop = FALSE])
    expect_error(b[, c(100, 2), drop = FALSE])

    expect_error(a[c(100, 2), c(100, 2)])
    expect_error(b[c(100, 2), c(100, 2)])
    expect_error(a[c(100, 2), c(100, 2), drop = TRUE])
    expect_error(b[c(100, 2), c(100, 2), drop = TRUE])
    expect_error(a[c(100, 2), c(100, 2), drop = FALSE])
    expect_error(b[c(100, 2), c(100, 2), drop = FALSE])

    expect_error(a[c(2, 100), ])
    expect_error(b[c(2, 100), ])
    expect_error(a[c(2, 100), , drop = TRUE])
    expect_error(b[c(2, 100), , drop = TRUE])
    expect_error(a[c(2, 100), , drop = FALSE])
    expect_error(b[c(2, 100), , drop = FALSE])

    expect_error(a[, c(2, 100)])
    expect_error(b[, c(2, 100)])
    expect_error(a[, c(2, 100), drop = TRUE])
    expect_error(b[, c(2, 100), drop = TRUE])
    expect_error(a[, c(2, 100), drop = FALSE])
    expect_error(b[, c(2, 100), drop = FALSE])

    expect_error(a[c(2, 100), c(2, 100)])
    expect_error(b[c(2, 100), c(2, 100)])
    expect_error(a[c(2, 100), c(2, 100), drop = TRUE])
    expect_error(b[c(2, 100), c(2, 100), drop = TRUE])
    expect_error(a[c(2, 100), c(2, 100), drop = FALSE])
    expect_error(b[c(2, 100), c(2, 100), drop = FALSE])

})

test_that("multi indexing by negative integers works", {

    expect_equal(a[c(-1), ], b[c(-1), ])
    expect_equal(a[c(-1), , drop = TRUE], b[c(-1), , drop = TRUE])
    expect_equal(a[c(-1), , drop = FALSE], b[c(-1), , drop = FALSE])

    expect_equal(a[, c(-1)], b[, c(-1)])
    expect_equal(a[, c(-1), drop = TRUE], b[, c(-1), drop = TRUE])
    expect_equal(a[, c(-1), drop = FALSE], b[, c(-1), drop = FALSE])

    expect_equal(a[c(-1), c(-1)], b[c(-1), c(-1)])
    expect_equal(a[c(-1), c(-1), drop = TRUE], b[c(-1), c(-1), drop = TRUE])
    expect_equal(a[c(-1), c(-1), drop = FALSE], b[c(-1), c(-1), drop = FALSE])

    expect_equal(a[c(-1, -2), ], b[c(-1, -2), ])
    expect_equal(a[c(-1, -2), , drop = TRUE], b[c(-1, -2), , drop = TRUE])
    expect_equal(a[c(-1, -2), , drop = FALSE], b[c(-1, -2), , drop = FALSE])

    expect_equal(a[, c(-1, -2)], b[, c(-1, -2)])
    expect_equal(a[, c(-1, -2), drop = TRUE], b[, c(-1, -2), drop = TRUE])
    expect_equal(a[, c(-1, -2), drop = FALSE], b[, c(-1, -2), drop = FALSE])

    expect_equal(a[c(-1, -2), c(-1, -2)], b[c(-1, -2), c(-1, -2)])
    expect_equal(a[c(-1, -2), c(-1, -2), drop = TRUE], b[c(-1, -2), c(-1, -2), drop = TRUE])
    expect_equal(a[c(-1, -2), c(-1, -2), drop = FALSE], b[c(-1, -2), c(-1, -2), drop = FALSE])

    expect_equal(a[c(-2, -1), ], b[c(-2, -1), ])
    expect_equal(a[c(-2, -1), , drop = TRUE], b[c(-2, -1), , drop = TRUE])
    expect_equal(a[c(-2, -1), , drop = FALSE], b[c(-2, -1), , drop = FALSE])

    expect_equal(a[, c(-2, -1)], b[, c(-2, -1)])
    expect_equal(a[, c(-2, -1), drop = TRUE], b[, c(-2, -1), drop = TRUE])
    expect_equal(a[, c(-2, -1), drop = FALSE], b[, c(-2, -1), drop = FALSE])

    expect_equal(a[c(-2, -1), c(-2, -1)], b[c(-2, -1), c(-2, -1)])
    expect_equal(a[c(-2, -1), c(-2, -1), drop = TRUE], b[c(-2, -1), c(-2, -1), drop = TRUE])
    expect_equal(a[c(-2, -1), c(-2, -1), drop = FALSE], b[c(-2, -1), c(-2, -1), drop = FALSE])

    expect_equal(a[c(-1.1), ], b[c(-1.1), ])
    expect_equal(a[c(-1.1), , drop = TRUE], b[c(-1.1), , drop = TRUE])
    expect_equal(a[c(-1.1), , drop = FALSE], b[c(-1.1), , drop = FALSE])

    expect_equal(a[, c(-1.1)], b[, c(-1.1)])
    expect_equal(a[, c(-1.1), drop = TRUE], b[, c(-1.1), drop = TRUE])
    expect_equal(a[, c(-1.1), drop = FALSE], b[, c(-1.1), drop = FALSE])

    expect_equal(a[c(-1.1), c(-1.1)], b[c(-1.1), c(-1.1)])
    expect_equal(a[c(-1.1), c(-1.1), drop = TRUE], b[c(-1.1), c(-1.1), drop = TRUE])
    expect_equal(a[c(-1.1), c(-1.1), drop = FALSE], b[c(-1.1), c(-1.1), drop = FALSE])

    expect_equal(a[c(-1.1, -2.1), ], b[c(-1.1, -2.1), ])
    expect_equal(a[c(-1.1, -2.1), , drop = TRUE], b[c(-1.1, -2.1), , drop = TRUE])
    expect_equal(a[c(-1.1, -2.1), , drop = FALSE], b[c(-1.1, -2.1), , drop = FALSE])

    expect_equal(a[, c(-1.1, -2.1)], b[, c(-1.1, -2.1)])
    expect_equal(a[, c(-1.1, -2.1), drop = TRUE], b[, c(-1.1, -2.1), drop = TRUE])
    expect_equal(a[, c(-1.1, -2.1), drop = FALSE], b[, c(-1.1, -2.1), drop = FALSE])

    expect_equal(a[c(-1.1, -2.1), c(-1.1, -2.1)], b[c(-1.1, -2.1), c(-1.1, -2.1)])
    expect_equal(a[c(-1.1, -2.1), c(-1.1, -2.1), drop = TRUE], b[c(-1.1, -2.1), c(-1.1, -2.1), drop = TRUE])
    expect_equal(a[c(-1.1, -2.1), c(-1.1, -2.1), drop = FALSE], b[c(-1.1, -2.1), c(-1.1, -2.1), drop = FALSE])

    expect_equal(a[c(-2.1, -1.1), ], b[c(-2.1, -1.1), ])
    expect_equal(a[c(-2.1, -1.1), , drop = TRUE], b[c(-2.1, -1.1), , drop = TRUE])
    expect_equal(a[c(-2.1, -1.1), , drop = FALSE], b[c(-2.1, -1.1), , drop = FALSE])

    expect_equal(a[, c(-2.1, -1.1)], b[, c(-2.1, -1.1)])
    expect_equal(a[, c(-2.1, -1.1), drop = TRUE], b[, c(-2.1, -1.1), drop = TRUE])
    expect_equal(a[, c(-2.1, -1.1), drop = FALSE], b[, c(-2.1, -1.1), drop = FALSE])

    expect_equal(a[c(-2.1, -1.1), c(-2.1, -1.1)], b[c(-2.1, -1.1), c(-2.1, -1.1)])
    expect_equal(a[c(-2.1, -1.1), c(-2.1, -1.1), drop = TRUE], b[c(-2.1, -1.1), c(-2.1, -1.1), drop = TRUE])
    expect_equal(a[c(-2.1, -1.1), c(-2.1, -1.1), drop = FALSE], b[c(-2.1, -1.1), c(-2.1, -1.1), drop = FALSE])

    expect_equal(a[c(-1.9), ], b[c(-1.9), ])
    expect_equal(a[c(-1.9), , drop = TRUE], b[c(-1.9), , drop = TRUE])
    expect_equal(a[c(-1.9), , drop = FALSE], b[c(-1.9), , drop = FALSE])

    expect_equal(a[, c(-1.9)], b[, c(-1.9)])
    expect_equal(a[, c(-1.9), drop = TRUE], b[, c(-1.9), drop = TRUE])
    expect_equal(a[, c(-1.9), drop = FALSE], b[, c(-1.9), drop = FALSE])

    expect_equal(a[c(-1.9), c(-1.9)], b[c(-1.9), c(-1.9)])
    expect_equal(a[c(-1.9), c(-1.9), drop = TRUE], b[c(-1.9), c(-1.9), drop = TRUE])
    expect_equal(a[c(-1.9), c(-1.9), drop = FALSE], b[c(-1.9), c(-1.9), drop = FALSE])

    expect_equal(a[c(-1.9, -2.9), ], b[c(-1.9, -2.9), ])
    expect_equal(a[c(-1.9, -2.9), , drop = TRUE], b[c(-1.9, -2.9), , drop = TRUE])
    expect_equal(a[c(-1.9, -2.9), , drop = FALSE], b[c(-1.9, -2.9), , drop = FALSE])

    expect_equal(a[, c(-1.9, -2.9)], b[, c(-1.9, -2.9)])
    expect_equal(a[, c(-1.9, -2.9), drop = TRUE], b[, c(-1.9, -2.9), drop = TRUE])
    expect_equal(a[, c(-1.9, -2.9), drop = FALSE], b[, c(-1.9, -2.9), drop = FALSE])

    expect_equal(a[c(-1.9, -2.9), c(-1.9, -2.9)], b[c(-1.9, -2.9), c(-1.9, -2.9)])
    expect_equal(a[c(-1.9, -2.9), c(-1.9, -2.9), drop = TRUE], b[c(-1.9, -2.9), c(-1.9, -2.9), drop = TRUE])
    expect_equal(a[c(-1.9, -2.9), c(-1.9, -2.9), drop = FALSE], b[c(-1.9, -2.9), c(-1.9, -2.9), drop = FALSE])

    expect_equal(a[c(-2.9, -1.9), ], b[c(-2.9, -1.9), ])
    expect_equal(a[c(-2.9, -1.9), , drop = TRUE], b[c(-2.9, -1.9), , drop = TRUE])
    expect_equal(a[c(-2.9, -1.9), , drop = FALSE], b[c(-2.9, -1.9), , drop = FALSE])

    expect_equal(a[, c(-2.9, -1.9)], b[, c(-2.9, -1.9)])
    expect_equal(a[, c(-2.9, -1.9), drop = TRUE], b[, c(-2.9, -1.9), drop = TRUE])
    expect_equal(a[, c(-2.9, -1.9), drop = FALSE], b[, c(-2.9, -1.9), drop = FALSE])

    expect_equal(a[c(-2.9, -1.9), c(-2.9, -1.9)], b[c(-2.9, -1.9), c(-2.9, -1.9)])
    expect_equal(a[c(-2.9, -1.9), c(-2.9, -1.9), drop = TRUE], b[c(-2.9, -1.9), c(-2.9, -1.9), drop = TRUE])
    expect_equal(a[c(-2.9, -1.9), c(-2.9, -1.9), drop = FALSE], b[c(-2.9, -1.9), c(-2.9, -1.9), drop = FALSE])

    expect_equal(a[c(-100), ], b[c(-100), ])
    expect_equal(a[c(-100), , drop = TRUE], b[c(-100), , drop = TRUE])
    expect_equal(a[c(-100), , drop = FALSE], b[c(-100), , drop = FALSE])

    expect_equal(a[, c(-100)], b[, c(-100)])
    expect_equal(a[, c(-100), drop = TRUE], b[, c(-100), drop = TRUE])
    expect_equal(a[, c(-100), drop = FALSE], b[, c(-100), drop = FALSE])

    expect_equal(a[c(-100), c(-100)], b[c(-100), c(-100)])
    expect_equal(a[c(-100), c(-100), drop = TRUE], b[c(-100), c(-100), drop = TRUE])
    expect_equal(a[c(-100), c(-100), drop = FALSE], b[c(-100), c(-100), drop = FALSE])

    expect_equal(a[c(-100, -2), ], b[c(-100, -2), ])
    expect_equal(a[c(-100, -2), , drop = TRUE], b[c(-100, -2), , drop = TRUE])
    expect_equal(a[c(-100, -2), , drop = FALSE], b[c(-100, -2), , drop = FALSE])

    expect_equal(a[, c(-100, -2)], b[, c(-100, -2)])
    expect_equal(a[, c(-100, -2), drop = TRUE], b[, c(-100, -2), drop = TRUE])
    expect_equal(a[, c(-100, -2), drop = FALSE], b[, c(-100, -2), drop = FALSE])

    expect_equal(a[c(-100, -2), c(-100, -2)], b[c(-100, -2), c(-100, -2)])
    expect_equal(a[c(-100, -2), c(-100, -2), drop = TRUE], b[c(-100, -2), c(-100, -2), drop = TRUE])
    expect_equal(a[c(-100, -2), c(-100, -2), drop = FALSE], b[c(-100, -2), c(-100, -2), drop = FALSE])

    expect_equal(a[c(-2, -100), ], b[c(-2, -100), ])
    expect_equal(a[c(-2, -100), , drop = TRUE], b[c(-2, -100), , drop = TRUE])
    expect_equal(a[c(-2, -100), , drop = FALSE], b[c(-2, -100), , drop = FALSE])

    expect_equal(a[, c(-2, -100)], b[, c(-2, -100)])
    expect_equal(a[, c(-2, -100), drop = TRUE], b[, c(-2, -100), drop = TRUE])
    expect_equal(a[, c(-2, -100), drop = FALSE], b[, c(-2, -100), drop = FALSE])

    expect_equal(a[c(-2, -100), c(-2, -100)], b[c(-2, -100), c(-2, -100)])
    expect_equal(a[c(-2, -100), c(-2, -100), drop = TRUE], b[c(-2, -100), c(-2, -100), drop = TRUE])
    expect_equal(a[c(-2, -100), c(-2, -100), drop = FALSE], b[c(-2, -100), c(-2, -100), drop = FALSE])

})

test_that("multi indexing by logicals works", {

    expect_equal(a[c(TRUE), ], b[c(TRUE), ])
    expect_equal(a[c(TRUE), , drop = TRUE], b[c(TRUE), , drop = TRUE])
    expect_equal(a[c(TRUE), , drop = FALSE], b[c(TRUE), , drop = FALSE])

    expect_equal(a[, c(TRUE)], b[, c(TRUE)])
    expect_equal(a[, c(TRUE), drop = TRUE], b[, c(TRUE), drop = TRUE])
    expect_equal(a[, c(TRUE), drop = FALSE], b[, c(TRUE), drop = FALSE])

    expect_equal(a[c(TRUE), c(TRUE)], b[c(TRUE), c(TRUE)])
    expect_equal(a[c(TRUE), c(TRUE), drop = TRUE], b[c(TRUE), c(TRUE), drop = TRUE])
    expect_equal(a[c(TRUE), c(TRUE), drop = FALSE], b[c(TRUE), c(TRUE), drop = FALSE])

    expect_error(a[c(rep_len(TRUE, 100)), ])
    expect_error(b[c(rep_len(TRUE, 100)), ])
    expect_error(a[c(rep_len(TRUE, 100)), , drop = rep_len(TRUE, 100)])
    expect_error(b[c(rep_len(TRUE, 100)), , drop = rep_len(TRUE, 100)])
    expect_error(a[c(rep_len(TRUE, 100)), , drop = FALSE])
    expect_error(b[c(rep_len(TRUE, 100)), , drop = FALSE])

    expect_error(a[, c(rep_len(TRUE, 100))])
    expect_error(b[, c(rep_len(TRUE, 100))])
    expect_error(a[, c(rep_len(TRUE, 100)), drop = rep_len(TRUE, 100)])
    expect_error(b[, c(rep_len(TRUE, 100)), drop = rep_len(TRUE, 100)])
    expect_error(a[, c(rep_len(TRUE, 100)), drop = FALSE])
    expect_error(b[, c(rep_len(TRUE, 100)), drop = FALSE])

    expect_error(a[c(rep_len(TRUE, 100)), c(rep_len(TRUE, 100))])
    expect_error(b[c(rep_len(TRUE, 100)), c(rep_len(TRUE, 100))])
    expect_error(a[c(rep_len(TRUE, 100)), c(rep_len(TRUE, 100)), drop = rep_len(TRUE, 100)])
    expect_error(b[c(rep_len(TRUE, 100)), c(rep_len(TRUE, 100)), drop = rep_len(TRUE, 100)])
    expect_error(a[c(rep_len(TRUE, 100)), c(rep_len(TRUE, 100)), drop = FALSE])
    expect_error(b[c(rep_len(TRUE, 100)), c(rep_len(TRUE, 100)), drop = FALSE])

    expect_equal(a[c(FALSE), ], b[c(FALSE), ])
    expect_equal(a[c(FALSE), , drop = FALSE], b[c(FALSE), , drop = FALSE])
    expect_equal(a[c(FALSE), , drop = FALSE], b[c(FALSE), , drop = FALSE])

    expect_equal(a[, c(FALSE)], b[, c(FALSE)])
    expect_equal(a[, c(FALSE), drop = FALSE], b[, c(FALSE), drop = FALSE])
    expect_equal(a[, c(FALSE), drop = FALSE], b[, c(FALSE), drop = FALSE])

    expect_equal(a[c(FALSE), c(FALSE)], b[c(FALSE), c(FALSE)])
    expect_equal(a[c(FALSE), c(FALSE), drop = FALSE], b[c(FALSE), c(FALSE), drop = FALSE])
    expect_equal(a[c(FALSE), c(FALSE), drop = FALSE], b[c(FALSE), c(FALSE), drop = FALSE])

    expect_error(a[c(rep_len(FALSE, 100)), ])
    expect_error(b[c(rep_len(FALSE, 100)), ])
    expect_error(a[c(rep_len(FALSE, 100)), , drop = rep_len(FALSE, 100)])
    expect_error(b[c(rep_len(FALSE, 100)), , drop = rep_len(FALSE, 100)])
    expect_error(a[c(rep_len(FALSE, 100)), , drop = rep_len(FALSE, 100)])
    expect_error(b[c(rep_len(FALSE, 100)), , drop = rep_len(FALSE, 100)])

    expect_error(a[, c(rep_len(FALSE, 100))])
    expect_error(b[, c(rep_len(FALSE, 100))])
    expect_error(a[, c(rep_len(FALSE, 100)), drop = rep_len(FALSE, 100)])
    expect_error(b[, c(rep_len(FALSE, 100)), drop = rep_len(FALSE, 100)])
    expect_error(a[, c(rep_len(FALSE, 100)), drop = rep_len(FALSE, 100)])
    expect_error(b[, c(rep_len(FALSE, 100)), drop = rep_len(FALSE, 100)])

    expect_error(a[c(rep_len(FALSE, 100)), c(rep_len(FALSE, 100))])
    expect_error(b[c(rep_len(FALSE, 100)), c(rep_len(FALSE, 100))])
    expect_error(a[c(rep_len(FALSE, 100)), c(rep_len(FALSE, 100)), drop = rep_len(FALSE, 100)])
    expect_error(b[c(rep_len(FALSE, 100)), c(rep_len(FALSE, 100)), drop = rep_len(FALSE, 100)])
    expect_error(a[c(rep_len(FALSE, 100)), c(rep_len(FALSE, 100)), drop = rep_len(FALSE, 100)])
    expect_error(b[c(rep_len(FALSE, 100)), c(rep_len(FALSE, 100)), drop = rep_len(FALSE, 100)])

    expect_equal(a[c(TRUE, FALSE), ], b[c(TRUE, FALSE), ])
    expect_equal(a[c(TRUE, FALSE), , drop = TRUE], b[c(TRUE, FALSE), , drop = TRUE])
    expect_equal(a[c(TRUE, FALSE), , drop = FALSE], b[c(TRUE, FALSE), , drop = FALSE])

    expect_equal(a[, c(TRUE, FALSE)], b[, c(TRUE, FALSE)])
    expect_equal(a[, c(TRUE, FALSE), drop = TRUE], b[, c(TRUE, FALSE), drop = TRUE])
    expect_equal(a[, c(TRUE, FALSE), drop = FALSE], b[, c(TRUE, FALSE), drop = FALSE])

    expect_equal(a[c(TRUE, FALSE), c(TRUE, FALSE)], b[c(TRUE, FALSE), c(TRUE, FALSE)])
    expect_equal(a[c(TRUE, FALSE), c(TRUE, FALSE), drop = TRUE], b[c(TRUE, FALSE), c(TRUE, FALSE), drop = TRUE])
    expect_equal(a[c(TRUE, FALSE), c(TRUE, FALSE), drop = FALSE], b[c(TRUE, FALSE), c(TRUE, FALSE), drop = FALSE])

    expect_error(a[rep_len(c(TRUE, FALSE), 100), ])
    expect_error(b[rep_len(c(TRUE, FALSE), 100), ])
    expect_error(a[rep_len(c(TRUE, FALSE), 100), , drop = TRUE])
    expect_error(b[rep_len(c(TRUE, FALSE), 100), , drop = TRUE])
    expect_error(a[rep_len(c(TRUE, FALSE), 100), , drop = FALSE])
    expect_error(b[rep_len(c(TRUE, FALSE), 100), , drop = FALSE])

    expect_error(a[, rep_len(c(TRUE, FALSE), 100)])
    expect_error(b[, rep_len(c(TRUE, FALSE), 100)])
    expect_error(a[, rep_len(c(TRUE, FALSE), 100), drop = TRUE])
    expect_error(b[, rep_len(c(TRUE, FALSE), 100), drop = TRUE])
    expect_error(a[, rep_len(c(TRUE, FALSE), 100), drop = FALSE])
    expect_error(b[, rep_len(c(TRUE, FALSE), 100), drop = FALSE])

    expect_error(a[rep_len(c(TRUE, FALSE), 100), rep_len(c(TRUE, FALSE), 100)])
    expect_error(b[rep_len(c(TRUE, FALSE), 100), rep_len(c(TRUE, FALSE), 100)])
    expect_error(a[rep_len(c(TRUE, FALSE), 100), rep_len(c(TRUE, FALSE), 100), drop = TRUE])
    expect_error(b[rep_len(c(TRUE, FALSE), 100), rep_len(c(TRUE, FALSE), 100), drop = TRUE])
    expect_error(a[rep_len(c(TRUE, FALSE), 100), rep_len(c(TRUE, FALSE), 100), drop = FALSE])
    expect_error(b[rep_len(c(TRUE, FALSE), 100), rep_len(c(TRUE, FALSE), 100), drop = FALSE])

    expect_equal(a[c(FALSE, TRUE), ], b[c(FALSE, TRUE), ])
    expect_equal(a[c(FALSE, TRUE), , drop = TRUE], b[c(FALSE, TRUE), , drop = TRUE])
    expect_equal(a[c(FALSE, TRUE), , drop = FALSE], b[c(FALSE, TRUE), , drop = FALSE])

    expect_equal(a[, c(FALSE, TRUE)], b[, c(FALSE, TRUE)])
    expect_equal(a[, c(FALSE, TRUE), drop = TRUE], b[, c(FALSE, TRUE), drop = TRUE])
    expect_equal(a[, c(FALSE, TRUE), drop = FALSE], b[, c(FALSE, TRUE), drop = FALSE])

    expect_equal(a[c(FALSE, TRUE), c(FALSE, TRUE)], b[c(FALSE, TRUE), c(FALSE, TRUE)])
    expect_equal(a[c(FALSE, TRUE), c(FALSE, TRUE), drop = TRUE], b[c(FALSE, TRUE), c(FALSE, TRUE), drop = TRUE])
    expect_equal(a[c(FALSE, TRUE), c(FALSE, TRUE), drop = FALSE], b[c(FALSE, TRUE), c(FALSE, TRUE), drop = FALSE])

    expect_error(a[rep_len(c(FALSE, TRUE), 100), ])
    expect_error(b[rep_len(c(FALSE, TRUE), 100), ])
    expect_error(a[rep_len(c(FALSE, TRUE), 100), , drop = TRUE])
    expect_error(b[rep_len(c(FALSE, TRUE), 100), , drop = TRUE])
    expect_error(a[rep_len(c(FALSE, TRUE), 100), , drop = FALSE])
    expect_error(b[rep_len(c(FALSE, TRUE), 100), , drop = FALSE])

    expect_error(a[, rep_len(c(FALSE, TRUE), 100)])
    expect_error(b[, rep_len(c(FALSE, TRUE), 100)])
    expect_error(a[, rep_len(c(FALSE, TRUE), 100), drop = TRUE])
    expect_error(b[, rep_len(c(FALSE, TRUE), 100), drop = TRUE])
    expect_error(a[, rep_len(c(FALSE, TRUE), 100), drop = FALSE])
    expect_error(b[, rep_len(c(FALSE, TRUE), 100), drop = FALSE])

    expect_error(a[rep_len(c(FALSE, TRUE), 100), rep_len(c(FALSE, TRUE), 100)])
    expect_error(b[rep_len(c(FALSE, TRUE), 100), rep_len(c(FALSE, TRUE), 100)])
    expect_error(a[rep_len(c(FALSE, TRUE), 100), rep_len(c(FALSE, TRUE), 100), drop = TRUE])
    expect_error(b[rep_len(c(FALSE, TRUE), 100), rep_len(c(FALSE, TRUE), 100), drop = TRUE])
    expect_error(a[rep_len(c(FALSE, TRUE), 100), rep_len(c(FALSE, TRUE), 100), drop = FALSE])
    expect_error(b[rep_len(c(FALSE, TRUE), 100), rep_len(c(FALSE, TRUE), 100), drop = FALSE])

})

test_that("multi indexing by characters works", {

    expect_equal(a["a", ], b["a", ])
    expect_equal(a["a", , drop = TRUE], b["a", , drop = TRUE])
    expect_equal(a["a", , drop = FALSE], b["a", , drop = FALSE])

    expect_equal(a[, "a"], b[, "a"])
    expect_equal(a[, "a", drop = TRUE], b[, "a", drop = TRUE])
    expect_equal(a[, "a", drop = FALSE], b[, "a", drop = FALSE])

    expect_equal(a["a", "a"], b["a", "a"])
    expect_equal(a["a", "a", drop = TRUE], b["a", "a", drop = TRUE])
    expect_equal(a["a", "a", drop = FALSE], b["a", "a", drop = FALSE])

    expect_error(a["x", ])
    expect_error(b["x", ])
    expect_error(a["x", , drop = TRUE])
    expect_error(b["x", , drop = TRUE])
    expect_error(a["x", , drop = FALSE])
    expect_error(b["x", , drop = FALSE])

    expect_error(a[, "x"])
    expect_error(b[, "x"])
    expect_error(a[, "x", drop = TRUE])
    expect_error(b[, "x", drop = TRUE])
    expect_error(a[, "x", drop = FALSE])
    expect_error(b[, "x", drop = FALSE])

    expect_error(a["x", "x"])
    expect_error(b["x", "x"])
    expect_error(a["x", "x", drop = TRUE])
    expect_error(b["x", "x", drop = TRUE])
    expect_error(a["x", "x", drop = FALSE])
    expect_error(b["x", "x", drop = FALSE])

    expect_equal(a[c("a", "b"), ], b[c("a", "b"), ])
    expect_equal(a[c("a", "b"), , drop = TRUE], b[c("a", "b"), , drop = TRUE])
    expect_equal(a[c("a", "b"), , drop = FALSE], b[c("a", "b"), , drop = FALSE])

    expect_equal(a[, c("a", "b")], b[, c("a", "b")])
    expect_equal(a[, c("a", "b"), drop = TRUE], b[, c("a", "b"), drop = TRUE])
    expect_equal(a[, c("a", "b"), drop = FALSE], b[, c("a", "b"), drop = FALSE])

    expect_equal(a[c("a", "b"), c("a", "b")], b[c("a", "b"), c("a", "b")])
    expect_equal(a[c("a", "b"), c("a", "b"), drop = TRUE], b[c("a", "b"), c("a", "b"), drop = TRUE])
    expect_equal(a[c("a", "b"), c("a", "b"), drop = FALSE], b[c("a", "b"), c("a", "b"), drop = FALSE])

    expect_equal(a[c("b", "a"), ], b[c("b", "a"), ])
    expect_equal(a[c("b", "a"), , drop = TRUE], b[c("b", "a"), , drop = TRUE])
    expect_equal(a[c("b", "a"), , drop = FALSE], b[c("b", "a"), , drop = FALSE])

    expect_equal(a[, c("b", "a")], b[, c("b", "a")])
    expect_equal(a[, c("b", "a"), drop = TRUE], b[, c("b", "a"), drop = TRUE])
    expect_equal(a[, c("b", "a"), drop = FALSE], b[, c("b", "a"), drop = FALSE])

    expect_equal(a[c("b", "a"), c("b", "a")], b[c("b", "a"), c("b", "a")])
    expect_equal(a[c("b", "a"), c("b", "a"), drop = TRUE], b[c("b", "a"), c("b", "a"), drop = TRUE])
    expect_equal(a[c("b", "a"), c("b", "a"), drop = FALSE], b[c("b", "a"), c("b", "a"), drop = FALSE])

    expect_error(a[c("a", "x"), ])
    expect_error(b[c("a", "x"), ])
    expect_error(a[c("a", "x"), , drop = TRUE])
    expect_error(b[c("a", "x"), , drop = TRUE])
    expect_error(a[c("a", "x"), , drop = FALSE])
    expect_error(b[c("a", "x"), , drop = FALSE])

    expect_error(a[, c("a", "x")])
    expect_error(b[, c("a", "x")])
    expect_error(a[, c("a", "x"), drop = TRUE])
    expect_error(b[, c("a", "x"), drop = TRUE])
    expect_error(a[, c("a", "x"), drop = FALSE])
    expect_error(b[, c("a", "x"), drop = FALSE])

    expect_error(a[c("a", "x"), c("a", "x")])
    expect_error(b[c("a", "x"), c("a", "x")])
    expect_error(a[c("a", "x"), c("a", "x"), drop = TRUE])
    expect_error(b[c("a", "x"), c("a", "x"), drop = TRUE])
    expect_error(a[c("a", "x"), c("a", "x"), drop = FALSE])
    expect_error(b[c("a", "x"), c("a", "x"), drop = FALSE])

    expect_error(a[c("x", "a"), ])
    expect_error(b[c("x", "a"), ])
    expect_error(a[c("x", "a"), , drop = TRUE])
    expect_error(b[c("x", "a"), , drop = TRUE])
    expect_error(a[c("x", "a"), , drop = FALSE])
    expect_error(b[c("x", "a"), , drop = FALSE])

    expect_error(a[, c("x", "a")])
    expect_error(b[, c("x", "a")])
    expect_error(a[, c("x", "a"), drop = TRUE])
    expect_error(b[, c("x", "a"), drop = TRUE])
    expect_error(a[, c("x", "a"), drop = FALSE])
    expect_error(b[, c("x", "a"), drop = FALSE])

    expect_error(a[c("x", "a"), c("x", "a")])
    expect_error(b[c("x", "a"), c("x", "a")])
    expect_error(a[c("x", "a"), c("x", "a"), drop = TRUE])
    expect_error(b[c("x", "a"), c("x", "a"), drop = TRUE])
    expect_error(a[c("x", "a"), c("x", "a"), drop = FALSE])
    expect_error(b[c("x", "a"), c("x", "a"), drop = FALSE])

})

test_that("multi indexing by NA works", {

    expect_equal(a[c(1, NA), ], b[c(1, NA), ])
    expect_equal(a[c(1, NA), , drop = TRUE], b[c(1, NA), , drop = TRUE])
    expect_equal(a[c(1, NA), , drop = FALSE], b[c(1, NA), , drop = FALSE])

    expect_equal(a[, c(1, NA)], b[, c(1, NA)])
    expect_equal(a[, c(1, NA), drop = TRUE], b[, c(1, NA), drop = TRUE])
    expect_equal(a[, c(1, NA), drop = FALSE], b[, c(1, NA), drop = FALSE])

    expect_equal(a[c(1, NA), c(1, NA)], b[c(1, NA), c(1, NA)])
    expect_equal(a[c(1, NA), c(1, NA), drop = TRUE], b[c(1, NA), c(1, NA), drop = TRUE])
    expect_equal(a[c(1, NA), c(1, NA), drop = FALSE], b[c(1, NA), c(1, NA), drop = FALSE])

    expect_error(a[c(-1, NA), ])
    expect_error(b[c(-1, NA), ])
    expect_error(a[c(-1, NA), , drop = TRUE])
    expect_error(b[c(-1, NA), , drop = TRUE])
    expect_error(a[c(-1, NA), , drop = FALSE])
    expect_error(b[c(-1, NA), , drop = FALSE])

    expect_error(a[, c(-1, NA)])
    expect_error(b[, c(-1, NA)])
    expect_error(a[, c(-1, NA), drop = TRUE])
    expect_error(b[, c(-1, NA), drop = TRUE])
    expect_error(a[, c(-1, NA), drop = FALSE])
    expect_error(b[, c(-1, NA), drop = FALSE])

    expect_error(a[c(-1, NA), c(-1, NA)])
    expect_error(b[c(-1, NA), c(-1, NA)])
    expect_error(a[c(-1, NA), c(-1, NA), drop = TRUE])
    expect_error(b[c(-1, NA), c(-1, NA), drop = TRUE])
    expect_error(a[c(-1, NA), c(-1, NA), drop = FALSE])
    expect_error(b[c(-1, NA), c(-1, NA), drop = FALSE])

    expect_equal(a[c(TRUE, NA), ], b[c(TRUE, NA), ])
    expect_equal(a[c(TRUE, NA), , drop = TRUE], b[c(TRUE, NA), , drop = TRUE])
    expect_equal(a[c(TRUE, NA), , drop = FALSE], b[c(TRUE, NA), , drop = FALSE])

    expect_equal(a[, c(TRUE, NA)], b[, c(TRUE, NA)])
    expect_equal(a[, c(TRUE, NA), drop = TRUE], b[, c(TRUE, NA), drop = TRUE])
    expect_equal(a[, c(TRUE, NA), drop = FALSE], b[, c(TRUE, NA), drop = FALSE])

    expect_equal(a[c(TRUE, NA), c(TRUE, NA)], b[c(TRUE, NA), c(TRUE, NA)])
    expect_equal(a[c(TRUE, NA), c(TRUE, NA), drop = TRUE], b[c(TRUE, NA), c(TRUE, NA), drop = TRUE])
    expect_equal(a[c(TRUE, NA), c(TRUE, NA), drop = FALSE], b[c(TRUE, NA), c(TRUE, NA), drop = FALSE])

    expect_equal(a[c(FALSE, NA), ], b[c(FALSE, NA), ])
    expect_equal(a[c(FALSE, NA), , drop = FALSE], b[c(FALSE, NA), , drop = FALSE])
    expect_equal(a[c(FALSE, NA), , drop = FALSE], b[c(FALSE, NA), , drop = FALSE])

    expect_equal(a[, c(FALSE, NA)], b[, c(FALSE, NA)])
    expect_equal(a[, c(FALSE, NA), drop = FALSE], b[, c(FALSE, NA), drop = FALSE])
    expect_equal(a[, c(FALSE, NA), drop = FALSE], b[, c(FALSE, NA), drop = FALSE])

    expect_equal(a[c(FALSE, NA), c(FALSE, NA)], b[c(FALSE, NA), c(FALSE, NA)])
    expect_equal(a[c(FALSE, NA), c(FALSE, NA), drop = FALSE], b[c(FALSE, NA), c(FALSE, NA), drop = FALSE])
    expect_equal(a[c(FALSE, NA), c(FALSE, NA), drop = FALSE], b[c(FALSE, NA), c(FALSE, NA), drop = FALSE])

    expect_error(a[c("a", NA), ])
    expect_error(b[c("a", NA), ])
    expect_error(a[c("a", NA), , drop = TRUE])
    expect_error(b[c("a", NA), , drop = TRUE])
    expect_error(a[c("a", NA), , drop = FALSE])
    expect_error(b[c("a", NA), , drop = FALSE])

    expect_error(a[, c("a", NA)])
    expect_error(b[, c("a", NA)])
    expect_error(a[, c("a", NA), drop = TRUE])
    expect_error(b[, c("a", NA), drop = TRUE])
    expect_error(a[, c("a", NA), drop = FALSE])
    expect_error(b[, c("a", NA), drop = FALSE])

    expect_error(a[c("a", NA), c("a", NA)])
    expect_error(b[c("a", NA), c("a", NA)])
    expect_error(a[c("a", NA), c("a", NA), drop = TRUE])
    expect_error(b[c("a", NA), c("a", NA), drop = TRUE])
    expect_error(a[c("a", NA), c("a", NA), drop = FALSE])
    expect_error(b[c("a", NA), c("a", NA), drop = FALSE])

    expect_equal(a[NA, ], b[NA, ])
    expect_equal(a[NA, , drop = TRUE], b[NA, , drop = TRUE])
    expect_equal(a[NA, , drop = FALSE], b[NA, , drop = FALSE])

    expect_equal(a[, NA], b[, NA])
    expect_equal(a[, NA, drop = TRUE], b[, NA, drop = TRUE])
    expect_equal(a[, NA, drop = FALSE], b[, NA, drop = FALSE])

    expect_equal(a[NA, NA], b[NA, NA])
    expect_equal(a[NA, NA, drop = TRUE], b[NA, NA, drop = TRUE])
    expect_equal(a[NA, NA, drop = FALSE], b[NA, NA, drop = FALSE])

    expect_equal(a[NA_integer_, ], b[NA_integer_, ])
    expect_equal(a[NA_integer_, , drop = TRUE], b[NA_integer_, , drop = TRUE])
    expect_equal(a[NA_integer_, , drop = FALSE], b[NA_integer_, , drop = FALSE])

    expect_equal(a[, NA_integer_], b[, NA_integer_])
    expect_equal(a[, NA_integer_, drop = TRUE], b[, NA_integer_, drop = TRUE])
    expect_equal(a[, NA_integer_, drop = FALSE], b[, NA_integer_, drop = FALSE])

    expect_equal(a[NA_integer_, NA_integer_], b[NA_integer_, NA_integer_])
    expect_equal(a[NA_integer_, NA_integer_, drop = TRUE], b[NA_integer_, NA_integer_, drop = TRUE])
    expect_equal(a[NA_integer_, NA_integer_, drop = FALSE], b[NA_integer_, NA_integer_, drop = FALSE])

    expect_error(a[NA_character_, ])
    expect_error(b[NA_character_, ])
    expect_error(a[NA_character_, , drop = TRUE])
    expect_error(b[NA_character_, , drop = TRUE])
    expect_error(a[NA_character_, , drop = FALSE])
    expect_error(b[NA_character_, , drop = FALSE])

    expect_error(a[, NA_character_])
    expect_error(b[, NA_character_])
    expect_error(a[, NA_character_, drop = TRUE])
    expect_error(b[, NA_character_, drop = TRUE])
    expect_error(a[, NA_character_, drop = FALSE])
    expect_error(b[, NA_character_, drop = FALSE])

    expect_error(a[NA_character_, NA_character_])
    expect_error(b[NA_character_, NA_character_])
    expect_error(a[NA_character_, NA_character_, drop = TRUE])
    expect_error(b[NA_character_, NA_character_, drop = TRUE])
    expect_error(a[NA_character_, NA_character_, drop = FALSE])
    expect_error(b[NA_character_, NA_character_, drop = FALSE])

})

test_that("multi indexing by zero works", {

    expect_error(a[0, 0]) # Not implemented
    expect_error(a[0, 0, drop = TRUE]) # Not implemented
    expect_error(a[0, 0, drop = FALSE]) # Not implemented

    expect_equal(a[c(0, 1), ], b[c(0, 1), ])
    expect_equal(a[c(0, 1), , drop = TRUE], b[c(0, 1), , drop = TRUE])
    expect_equal(a[c(0, 1), , drop = FALSE], b[c(0, 1), , drop = FALSE])

    expect_equal(a[, c(0, 1)], b[, c(0, 1)])
    expect_equal(a[, c(0, 1), drop = TRUE], b[, c(0, 1), drop = TRUE])
    expect_equal(a[, c(0, 1), drop = FALSE], b[, c(0, 1), drop = FALSE])

    expect_equal(a[c(0, 1), c(0, 1)], b[c(0, 1), c(0, 1)])
    expect_equal(a[c(0, 1), c(0, 1), drop = TRUE], b[c(0, 1), c(0, 1), drop = TRUE])
    expect_equal(a[c(0, 1), c(0, 1), drop = FALSE], b[c(0, 1), c(0, 1), drop = FALSE])

    expect_equal(a[c(0, -1), ], b[c(0, -1), ])
    expect_equal(a[c(0, -1), , drop = TRUE], b[c(0, -1), , drop = TRUE])
    expect_equal(a[c(0, -1), , drop = FALSE], b[c(0, -1), , drop = FALSE])

    expect_equal(a[, c(0, -1)], b[, c(0, -1)])
    expect_equal(a[, c(0, -1), drop = TRUE], b[, c(0, -1), drop = TRUE])
    expect_equal(a[, c(0, -1), drop = FALSE], b[, c(0, -1), drop = FALSE])

    expect_equal(a[c(0, -1), c(0, -1)], b[c(0, -1), c(0, -1)])
    expect_equal(a[c(0, -1), c(0, -1), drop = TRUE], b[c(0, -1), c(0, -1), drop = TRUE])
    expect_equal(a[c(0, -1), c(0, -1), drop = FALSE], b[c(0, -1), c(0, -1), drop = FALSE])

    expect_error(a[c(0, 1, -1), ])
    expect_error(b[c(0, 1, -1), ])
    expect_error(a[c(0, 1, -1), , drop = TRUE])
    expect_error(b[c(0, 1, -1), , drop = TRUE])
    expect_error(a[c(0, 1, -1), , drop = FALSE])
    expect_error(b[c(0, 1, -1), , drop = FALSE])

    expect_error(a[, c(0, 1, -1)])
    expect_error(b[, c(0, 1, -1)])
    expect_error(a[, c(0, 1, -1), drop = TRUE])
    expect_error(b[, c(0, 1, -1), drop = TRUE])
    expect_error(a[, c(0, 1, -1), drop = FALSE])
    expect_error(b[, c(0, 1, -1), drop = FALSE])

    expect_error(a[c(0, 1, -1), c(0, 1, -1)])
    expect_error(b[c(0, 1, -1), c(0, 1, -1)])
    expect_error(a[c(0, 1, -1), c(0, 1, -1), drop = TRUE])
    expect_error(b[c(0, 1, -1), c(0, 1, -1), drop = TRUE])
    expect_error(a[c(0, 1, -1), c(0, 1, -1), drop = FALSE])
    expect_error(b[c(0, 1, -1), c(0, 1, -1), drop = FALSE])

})
