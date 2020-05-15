# This file contains tests (for use with the R packages 'testthat' or
# 'tinytest') to check if a custom type that implements extraction using the
# 'extract' function behaves similarly to a regular R matrix.
#
# To run these tests as part your test suite for your custom type, prepare an
# environment that contains the following names: 'CUSTOM_OBJECT' (an instance
# of your custom type that implements extraction using the 'extract' function
# containing dummy data with at least 3 rows and 3 columns), 'COMPARE_OBJECT'
# (a regular matrix that represents the same dummy data as your object),
# 'OUT_OF_BOUNDS_INT' (an integer that represents an out of bounds value for
# your object in integer indexing), 'OUT_OF_BOUNDS_CHAR' (a string that
# represents an out of bounds value for your object in character indexing), and
# optionally 'SKIP_NA_TESTS' (a boolean whether to skip missing value tests or
# not). Once the environment is prepared, the tests can be sourced using
# 'source(system.file("test-suite", "crochet-extract.R", package = "crochet"),
# local = env)' where 'env' is the environment.
#
# See inst/tinytest/test-stringmatrix.R, the BEDMatrix package, or the
# LinkedMatrix package for examples.

# Environment needs to be passed via 'local' argument of 'source'.
if (!exists("CROCHET_EXTRACT_ENV", inherits = FALSE)) {

# lengths are the same
expect_equal(length(CUSTOM_OBJECT), length(COMPARE_OBJECT))

# dims are the same
expect_equal(dim(CUSTOM_OBJECT), dim(COMPARE_OBJECT))

# dimnames are the same
expect_equal(dimnames(CUSTOM_OBJECT), dimnames(COMPARE_OBJECT))

# minimum dimension requirement are met
expect_true(nrow(CUSTOM_OBJECT) > 3)
expect_true(ncol(CUSTOM_OBJECT) > 3)
expect_true(length(CUSTOM_OBJECT) > 3)

# maximum dimension requirement are met
expect_true(nrow(CUSTOM_OBJECT) < OUT_OF_BOUNDS_INT)
expect_true(ncol(CUSTOM_OBJECT) < OUT_OF_BOUNDS_INT)
expect_true(length(CUSTOM_OBJECT) < OUT_OF_BOUNDS_INT)

test_subsetting <- function(...) {

    # Check default drop behavior
    expect_equal(
        CUSTOM_OBJECT[...],
        COMPARE_OBJECT[...],
        info = paste0("INFO: ", deparse(match.call()))
    )

    # Check explicit drop = TRUE
    expect_equal(
        CUSTOM_OBJECT[..., drop = TRUE],
        COMPARE_OBJECT[..., drop = TRUE],
        info = paste0("INFO: ", deparse(match.call()), " (with drop = TRUE)")
    )

    # Check explicit drop = FALSE
    expect_equal(
        CUSTOM_OBJECT[..., drop = FALSE],
        COMPARE_OBJECT[..., drop = FALSE],
        info = paste0("INFO: ", deparse(match.call()), " (with drop = FALSE)")
    )

}

test_subsetting_error <- function(...) {

    # Check default drop behavior
    expect_error(
        CUSTOM_OBJECT[...],
        info = paste0("INFO: ", deparse(match.call()))
    )
    expect_error(
        COMPARE_OBJECT[...],
        info = paste0("INFO: ", deparse(match.call()))
    )

    # Check explicit drop = TRUE
    expect_error(
        CUSTOM_OBJECT[..., drop = TRUE],
        info = paste0("INFO: ", deparse(match.call()), " (with drop = TRUE)")
    )
    expect_error(
        COMPARE_OBJECT[..., drop = TRUE],
        info = paste0("INFO: ", deparse(match.call()), " (with drop = TRUE)")
    )

    # Check explicit drop = FALSE
    expect_error(
        CUSTOM_OBJECT[..., drop = FALSE],
        info = paste0("INFO: ", deparse(match.call()), " (with drop = FALSE)")
    )
    expect_error(
        COMPARE_OBJECT[..., drop = FALSE],
        info = paste0("INFO: ", deparse(match.call()), " (with drop = FALSE)")
    )

}

# single indexing by nothing
test_subsetting()

# single indexing by positive integers
test_subsetting(1)
test_subsetting(c(1, 2))
test_subsetting(c(2, 1))
test_subsetting(1.1)
test_subsetting(c(1.1, 2.1))
test_subsetting(c(2.1, 1.1))
test_subsetting(1.9)
test_subsetting(c(1.9, 2.9))
test_subsetting(c(2.9, 1.9))
test_subsetting(OUT_OF_BOUNDS_INT)
test_subsetting(c(OUT_OF_BOUNDS_INT, 2))
test_subsetting(c(2, OUT_OF_BOUNDS_INT))
m <- matrix(data = c(1, 1, 2, 2), ncol = 2, byrow = TRUE)
test_subsetting(m)
m <- matrix(data = c(1, 1, 2, 2), ncol = 2, byrow = TRUE)
test_subsetting(m)
m <- matrix(data = c(2, 2, 1, 1), ncol = 2, byrow = TRUE)
test_subsetting(m)
m <- matrix(data = c(1.1, 1.1, 2.1, 2.1), ncol = 2, byrow = TRUE)
test_subsetting(m)
m <- matrix(data = c(2.1, 2.1, 1.1, 1.1), ncol = 2, byrow = TRUE)
test_subsetting(m)
m <- matrix(data = c(1.9, 1.9, 2.9, 2.9), ncol = 2, byrow = TRUE)
test_subsetting(m)
m <- matrix(data = c(2.9, 2.9, 1.9, 1.9), ncol = 2, byrow = TRUE)
test_subsetting(m)
m <- matrix(data = c(OUT_OF_BOUNDS_INT, OUT_OF_BOUNDS_INT), ncol = 2, byrow = TRUE)
test_subsetting_error(m)

# single indexing by negative integers
test_subsetting(-1)
test_subsetting(c(-1, -2))
test_subsetting(c(-2, -1))
test_subsetting(-1.1)
test_subsetting(c(-1.1, -2.1))
test_subsetting(c(-2.1, -1.1))
test_subsetting(-1.9)
test_subsetting(c(-1.9, -2.9))
test_subsetting(c(-2.9, -1.9))
test_subsetting(-OUT_OF_BOUNDS_INT)
test_subsetting(c(-OUT_OF_BOUNDS_INT, -2))
test_subsetting(c(-2, -OUT_OF_BOUNDS_INT))

# single indexing by logicals
test_subsetting(TRUE)
test_subsetting(FALSE)
test_subsetting(c(TRUE, FALSE))
test_subsetting(c(FALSE, TRUE))
test_subsetting(rep_len(TRUE, OUT_OF_BOUNDS_INT))
test_subsetting(rep_len(FALSE, OUT_OF_BOUNDS_INT))
test_subsetting(rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT))
test_subsetting(rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT))
m <- matrix(data = rnorm(length(CUSTOM_OBJECT)), nrow = nrow(CUSTOM_OBJECT), ncol = ncol(CUSTOM_OBJECT))
test_subsetting(m > 1)

# single indexing by characters
if (is.null(dimnames(CUSTOM_OBJECT))) {
    message("skipping character indexing because dimnames are NULL")
} else {
    ROW_NAME_1 <- rownames(CUSTOM_OBJECT)[1]
    ROW_NAME_2 <- rownames(CUSTOM_OBJECT)[2]
    COL_NAME_1 <- colnames(CUSTOM_OBJECT)[1]
    COL_NAME_2 <- colnames(CUSTOM_OBJECT)[2]
    test_subsetting(ROW_NAME_1)
    test_subsetting(OUT_OF_BOUNDS_CHAR)
    m <- matrix(data = c(ROW_NAME_1, COL_NAME_1, ROW_NAME_2, COL_NAME_2), ncol = 2, byrow = TRUE)
    test_subsetting(m)
    m <- matrix(data = c(ROW_NAME_2, COL_NAME_2, ROW_NAME_1, COL_NAME_1), ncol = 2, byrow = TRUE)
    test_subsetting(m)
    m <- matrix(data = c(OUT_OF_BOUNDS_CHAR, OUT_OF_BOUNDS_CHAR), ncol = 2, byrow = TRUE)
    test_subsetting_error(m)
}

# single indexing by NA
if (exists("SKIP_NA_TESTS", inherits = FALSE) && SKIP_NA_TESTS) {
    message("skipping NA tests because tests were explicitly disabled")
} else {
    ROW_NAME_1 <- rownames(CUSTOM_OBJECT)[1]
    test_subsetting(NA)
    test_subsetting(NA_integer_)
    test_subsetting(NA_character_)
    test_subsetting(c(1, NA))
    test_subsetting_error(c(-1, NA))
    test_subsetting(c(TRUE, NA))
    test_subsetting(c(FALSE, NA))
    test_subsetting(c(ROW_NAME_1, NA))
}

# single indexing by zero
# test_subsetting_error(0) # Not implemented
test_subsetting(c(0, 1))
test_subsetting(c(0, -1))
test_subsetting_error(c(0, 1, -1))

# multi indexing by nothing
test_subsetting(, )

# multi indexing by positive integers
test_subsetting(1, )
test_subsetting(, 1)
test_subsetting(1, 1)
test_subsetting(c(1, 2), )
test_subsetting(, c(1, 2))
test_subsetting(c(1, 2), c(1, 2))
test_subsetting(c(2, 1), )
test_subsetting(, c(2, 1))
test_subsetting(c(2, 1), c(2, 1))
test_subsetting(1.1, )
test_subsetting(, 1.1)
test_subsetting(1.1, 1.1)
test_subsetting(c(1.1, 2.1), )
test_subsetting(, c(1.1, 2.1))
test_subsetting(c(1.1, 2.1), c(1.1, 2.1))
test_subsetting(c(2.1, 1.1), )
test_subsetting(, c(2.1, 1.1))
test_subsetting(c(2.1, 1.1), c(2.1, 1.1))
test_subsetting(1.9, )
test_subsetting(, 1.9)
test_subsetting(1.9, 1.9)
test_subsetting(c(1.9, 2.9), )
test_subsetting(, c(1.9, 2.9))
test_subsetting(c(1.9, 2.9), c(1.9, 2.9))
test_subsetting(c(2.9, 1.9), )
test_subsetting(, c(2.9, 1.9))
test_subsetting(c(2.9, 1.9), c(2.9, 1.9))
test_subsetting_error(OUT_OF_BOUNDS_INT, )
test_subsetting_error(, OUT_OF_BOUNDS_INT)
test_subsetting_error(OUT_OF_BOUNDS_INT, OUT_OF_BOUNDS_INT)
test_subsetting_error(c(OUT_OF_BOUNDS_INT, 2), )
test_subsetting_error(, c(OUT_OF_BOUNDS_INT, 2))
test_subsetting_error(c(OUT_OF_BOUNDS_INT, 2), c(OUT_OF_BOUNDS_INT, 2))
test_subsetting_error(c(2, OUT_OF_BOUNDS_INT), )
test_subsetting_error(, c(2, OUT_OF_BOUNDS_INT))
test_subsetting_error(c(2, OUT_OF_BOUNDS_INT), c(2, OUT_OF_BOUNDS_INT))

# multi indexing by negative integers
test_subsetting(c(-1), )
test_subsetting(, c(-1))
test_subsetting(c(-1), c(-1))
test_subsetting(c(-1, -2), )
test_subsetting(, c(-1, -2))
test_subsetting(c(-1, -2), c(-1, -2))
test_subsetting(c(-2, -1), )
test_subsetting(, c(-2, -1))
test_subsetting(c(-2, -1), c(-2, -1))
test_subsetting(c(-1.1), )
test_subsetting(, c(-1.1))
test_subsetting(c(-1.1), c(-1.1))
test_subsetting(c(-1.1, -2.1), )
test_subsetting(, c(-1.1, -2.1))
test_subsetting(c(-1.1, -2.1), c(-1.1, -2.1))
test_subsetting(c(-2.1, -1.1), )
test_subsetting(, c(-2.1, -1.1))
test_subsetting(c(-2.1, -1.1), c(-2.1, -1.1))
test_subsetting(c(-1.9), )
test_subsetting(, c(-1.9))
test_subsetting(c(-1.9), c(-1.9))
test_subsetting(c(-1.9, -2.9), )
test_subsetting(, c(-1.9, -2.9))
test_subsetting(c(-1.9, -2.9), c(-1.9, -2.9))
test_subsetting(c(-2.9, -1.9), )
test_subsetting(, c(-2.9, -1.9))
test_subsetting(c(-2.9, -1.9), c(-2.9, -1.9))
test_subsetting(c(-OUT_OF_BOUNDS_INT), )
test_subsetting(, c(-OUT_OF_BOUNDS_INT))
test_subsetting(c(-OUT_OF_BOUNDS_INT), c(-OUT_OF_BOUNDS_INT))
test_subsetting(c(-OUT_OF_BOUNDS_INT, -2), )
test_subsetting(, c(-OUT_OF_BOUNDS_INT, -2))
test_subsetting(c(-OUT_OF_BOUNDS_INT, -2), c(-OUT_OF_BOUNDS_INT, -2))
test_subsetting(c(-2, -OUT_OF_BOUNDS_INT), )
test_subsetting(, c(-2, -OUT_OF_BOUNDS_INT))
test_subsetting(c(-2, -OUT_OF_BOUNDS_INT), c(-2, -OUT_OF_BOUNDS_INT))

# multi indexing by logicals
test_subsetting(c(TRUE), )
test_subsetting(, c(TRUE))
test_subsetting(c(TRUE), c(TRUE))
test_subsetting_error(c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), )
test_subsetting_error(, c(rep_len(TRUE, OUT_OF_BOUNDS_INT)))
test_subsetting_error(c(rep_len(TRUE, OUT_OF_BOUNDS_INT)), c(rep_len(TRUE, OUT_OF_BOUNDS_INT)))
test_subsetting(c(FALSE), )
test_subsetting(, c(FALSE))
test_subsetting(c(FALSE), c(FALSE))
test_subsetting_error(c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), )
test_subsetting_error(, c(rep_len(FALSE, OUT_OF_BOUNDS_INT)))
test_subsetting_error(c(rep_len(FALSE, OUT_OF_BOUNDS_INT)), c(rep_len(FALSE, OUT_OF_BOUNDS_INT)))
test_subsetting(c(TRUE, FALSE), )
test_subsetting(, c(TRUE, FALSE))
test_subsetting(c(TRUE, FALSE), c(TRUE, FALSE))
test_subsetting_error(rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), )
test_subsetting_error(, rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT))
test_subsetting_error(rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT), rep_len(c(TRUE, FALSE), OUT_OF_BOUNDS_INT))
test_subsetting(c(FALSE, TRUE), )
test_subsetting(, c(FALSE, TRUE))
test_subsetting(c(FALSE, TRUE), c(FALSE, TRUE))
test_subsetting_error(rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), )
test_subsetting_error(, rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT))
test_subsetting_error(rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT), rep_len(c(FALSE, TRUE), OUT_OF_BOUNDS_INT))

# multi indexing by characters
if (is.null(dimnames(CUSTOM_OBJECT))) {
    message("skipping character indexing because dimnames are NULL")
} else {
    ROW_NAME_1 <- rownames(CUSTOM_OBJECT)[1]
    ROW_NAME_2 <- rownames(CUSTOM_OBJECT)[2]
    COL_NAME_1 <- colnames(CUSTOM_OBJECT)[1]
    COL_NAME_2 <- colnames(CUSTOM_OBJECT)[2]
    test_subsetting(ROW_NAME_1, )
    test_subsetting(, COL_NAME_1)
    test_subsetting(ROW_NAME_1, COL_NAME_1)
    test_subsetting_error(OUT_OF_BOUNDS_CHAR, )
    test_subsetting_error(, OUT_OF_BOUNDS_CHAR)
    test_subsetting_error(OUT_OF_BOUNDS_CHAR, OUT_OF_BOUNDS_CHAR)
    test_subsetting(c(ROW_NAME_1, ROW_NAME_2), )
    test_subsetting(, c(COL_NAME_1, COL_NAME_2))
    test_subsetting(c(ROW_NAME_1, ROW_NAME_2), c(COL_NAME_1, COL_NAME_2))
    test_subsetting(c(ROW_NAME_2, ROW_NAME_1), )
    test_subsetting(, c(COL_NAME_2, COL_NAME_1))
    test_subsetting(c(ROW_NAME_2, ROW_NAME_1), c(COL_NAME_2, COL_NAME_1))
    test_subsetting_error(c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), )
    test_subsetting_error(, c(COL_NAME_1, OUT_OF_BOUNDS_CHAR))
    test_subsetting_error(c(ROW_NAME_1, OUT_OF_BOUNDS_CHAR), c(COL_NAME_1, OUT_OF_BOUNDS_CHAR))
    test_subsetting_error(c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), )
    test_subsetting_error(, c(OUT_OF_BOUNDS_CHAR, COL_NAME_1))
    test_subsetting_error(c(OUT_OF_BOUNDS_CHAR, ROW_NAME_1), c(OUT_OF_BOUNDS_CHAR, COL_NAME_1))
}

# multi indexing by NA
if (exists("SKIP_NA_TESTS", inherits = FALSE) && SKIP_NA_TESTS) {
    message("skipping NA tests because tests were explicitly disabled")
} else {
    ROW_NAME_1 <- rownames(CUSTOM_OBJECT)[1]
    COL_NAME_1 <- colnames(CUSTOM_OBJECT)[1]
    test_subsetting(NA, )
    test_subsetting(, NA)
    test_subsetting(NA, NA)
    test_subsetting(NA_integer_, )
    test_subsetting(, NA_integer_)
    test_subsetting(NA_integer_, NA_integer_)
    test_subsetting_error(NA_character_, )
    test_subsetting_error(, NA_character_)
    test_subsetting_error(NA_character_, NA_character_)
    test_subsetting(c(1, NA), )
    test_subsetting(, c(1, NA))
    test_subsetting(c(1, NA), c(1, NA))
    test_subsetting_error(c(-1, NA), )
    test_subsetting_error(, c(-1, NA))
    test_subsetting_error(c(-1, NA), c(-1, NA))
    test_subsetting(c(TRUE, NA), )
    test_subsetting(, c(TRUE, NA))
    test_subsetting(c(TRUE, NA), c(TRUE, NA))
    test_subsetting(c(FALSE, NA), )
    test_subsetting(, c(FALSE, NA))
    test_subsetting(c(FALSE, NA), c(FALSE, NA))
    test_subsetting_error(c(ROW_NAME_1, NA), )
    test_subsetting_error(, c(COL_NAME_1, NA))
    test_subsetting_error(c(ROW_NAME_1, NA), c(COL_NAME_1, NA))
}

# multi indexing by zero
# test_subsetting_error(0, 0) # Not implemented
test_subsetting(c(0, 1), )
test_subsetting(, c(0, 1))
test_subsetting(c(0, 1), c(0, 1))
test_subsetting(c(0, -1), )
test_subsetting(, c(0, -1))
test_subsetting(c(0, -1), c(0, -1))
test_subsetting_error(c(0, 1, -1), )
test_subsetting_error(, c(0, 1, -1))
test_subsetting_error(c(0, 1, -1), c(0, 1, -1))

# n-dimensional indexing causes an error
test_subsetting_error(1, 1, 1)

}
