# This file contains tests (for use with the R packages 'testthat' or
# 'tinytest') to check if a custom type that implements replacement using the
# 'replace' function behaves similarly to a regular R matrix.
#
# To run these tests as part your test suite for your custom type, prepare an
# environment called 'CROCHET_REPLACE_ENV' that contains the following names:
# 'CUSTOM_OBJECT' (an instance of your custom type that implements replacement
# using the 'replace' function containing dummy data with at least 3 rows and 3
# columns), 'COMPARE_OBJECT' (a regular matrix that represents the same dummy
# data as your object), 'OUT_OF_BOUNDS_INT' (an integer that represents an out
# of bounds value for your object in integer indexing), 'OUT_OF_BOUNDS_CHAR' (a
# string that represents an out of bounds value for your object in character
# indexing), 'VALUE_POOL' (a vector containing possible values that your custom
# type can take on), and 'RESET' (a function that resets 'CUSTOM_OBJECT' and
# 'COMPARE_OBJECT' to their original state). Once the environment is prepared,
# the tests can be sourced using 'source(system.file("test-suite",
# "crochet-replace.R", package = "crochet"), local = TRUE)'.
#
# See tests/testthat/test-stringmatrix.R, the BEDMatrix package, or the
# LinkedMatrix package for examples.

# dims are the same
expect_equal(dim(CROCHET_REPLACE_ENV$CUSTOM_OBJECT), dim(CROCHET_REPLACE_ENV$COMPARE_OBJECT))

# dimnames are the same
expect_equal(dimnames(CROCHET_REPLACE_ENV$CUSTOM_OBJECT), dimnames(CROCHET_REPLACE_ENV$COMPARE_OBJECT))

# minimum dimension requirement are met
expect_true(nrow(CROCHET_REPLACE_ENV$CUSTOM_OBJECT) > 3)
expect_true(ncol(CROCHET_REPLACE_ENV$CUSTOM_OBJECT) > 3)
expect_true(length(CROCHET_REPLACE_ENV$CUSTOM_OBJECT) > 3)

# maximum dimension requirement are met
expect_true(nrow(CROCHET_REPLACE_ENV$CUSTOM_OBJECT) < CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT)
expect_true(ncol(CROCHET_REPLACE_ENV$CUSTOM_OBJECT) < CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT)
expect_true(length(CROCHET_REPLACE_ENV$CUSTOM_OBJECT) < CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT)

# both matrices are the same
expect_equal(CROCHET_REPLACE_ENV$CUSTOM_OBJECT[], CROCHET_REPLACE_ENV$COMPARE_OBJECT[])

test_replacement <- function(..., value) {
    CROCHET_REPLACE_ENV$CUSTOM_OBJECT[...] <- value
    CROCHET_REPLACE_ENV$COMPARE_OBJECT[...] <- value
    expect_equal(CROCHET_REPLACE_ENV$CUSTOM_OBJECT[], CROCHET_REPLACE_ENV$COMPARE_OBJECT[], info = paste0("INFO: ", deparse(match.call())))
    CROCHET_REPLACE_ENV$RESET()
}

test_replacement_warning <- function(..., value) {
    expect_warning(CROCHET_REPLACE_ENV$CUSTOM_OBJECT[...] <- value, info = paste0("INFO: ", deparse(match.call())))
    expect_warning(CROCHET_REPLACE_ENV$COMPARE_OBJECT[...] <- value, info = paste0("INFO: ", deparse(match.call())))
    expect_equal(CROCHET_REPLACE_ENV$CUSTOM_OBJECT[], CROCHET_REPLACE_ENV$COMPARE_OBJECT[], info = paste0("INFO: ", deparse(match.call())))
    CROCHET_REPLACE_ENV$RESET()
}

test_replacement_error <- function(..., value) {
    expect_error(CROCHET_REPLACE_ENV$CUSTOM_OBJECT[...] <- value, info = paste0("INFO: ", deparse(match.call())))
    expect_error(CROCHET_REPLACE_ENV$COMPARE_OBJECT[...] <- value, info = paste0("INFO: ", deparse(match.call())))
    CROCHET_REPLACE_ENV$RESET()
}

test_replacement_not_implemented <- function(..., value) {
    expect_error(CROCHET_REPLACE_ENV$CUSTOM_OBJECT[...] <- value, "not implemented", info = paste0("INFO: ", deparse(match.call())))
    CROCHET_REPLACE_ENV$RESET()
}

length <- length(CROCHET_REPLACE_ENV$CUSTOM_OBJECT)
values <- CROCHET_REPLACE_ENV$VALUE_POOL
value <- values[1]

# single replacement by nothing
test_replacement_error(value = values[0])

# single replacement by positive integers
test_replacement(1, value = value)
test_replacement(c(1, 2), value = value)
test_replacement(c(2, 1), value = value)
test_replacement(1.1, value = value)
test_replacement(c(1.1, 2.1), value = value)
test_replacement(c(2.1, 1.1), value = value)
test_replacement(1.9, value = value)
test_replacement(c(1.9, 2.9), value = value)
test_replacement(c(2.9, 1.9), value = value)
m <- matrix(data = c(1, 1, 2, 2), ncol = 2, byrow = TRUE)
test_replacement(m, value = value)
m <- matrix(data = c(1, 1, 2, 2), ncol = 2, byrow = TRUE)
test_replacement(m, value = value)
m <- matrix(data = c(2, 2, 1, 1), ncol = 2, byrow = TRUE)
test_replacement(m, value = value)
m <- matrix(data = c(1.1, 1.1, 2.1, 2.1), ncol = 2, byrow = TRUE)
test_replacement(m, value = value)
m <- matrix(data = c(2.1, 2.1, 1.1, 1.1), ncol = 2, byrow = TRUE)
test_replacement(m, value = value)
m <- matrix(data = c(1.9, 1.9, 2.9, 2.9), ncol = 2, byrow = TRUE)
test_replacement(m, value = value)
m <- matrix(data = c(2.9, 2.9, 1.9, 1.9), ncol = 2, byrow = TRUE)
test_replacement(m, value = value)
test_replacement(c(1, NA), value = value)

# single replacement by negative integers
test_replacement(-1, value = value)
test_replacement(c(-1, -2), value = value)
test_replacement(c(-2, -1), value = value)
test_replacement(-1.1, value = value)
test_replacement(c(-1.1, -2.1), value = value)
test_replacement(c(-2.1, -1.1), value = value)
test_replacement(-1.9, value = value)
test_replacement(c(-1.9, -2.9), value = value)
test_replacement(c(-2.9, -1.9), value = value)
test_replacement_error(c(-1, NA), value = value)

# single replacement by logicals
test_replacement(TRUE, value = value)
test_replacement(FALSE, value = value)
test_replacement(c(TRUE, FALSE), value = value)
test_replacement(c(FALSE, TRUE), value = value)
m <- matrix(data = rnorm(length), nrow = nrow(CROCHET_REPLACE_ENV$CUSTOM_OBJECT), ncol = ncol(CROCHET_REPLACE_ENV$CUSTOM_OBJECT))
test_replacement(m > 1, value = value)
test_replacement(c(TRUE, NA), value = value)
test_replacement(c(FALSE, NA), value = value)

# single replacement by characters
if (is.null(dimnames(CROCHET_REPLACE_ENV$CUSTOM_OBJECT))) {
    message("skipping character replacement because dimnames are NULL")
} else {
    ROW_NAME_1 <- rownames(CROCHET_REPLACE_ENV$CUSTOM_OBJECT)[1]
    ROW_NAME_2 <- rownames(CROCHET_REPLACE_ENV$CUSTOM_OBJECT)[2]
    COL_NAME_1 <- colnames(CROCHET_REPLACE_ENV$CUSTOM_OBJECT)[1]
    COL_NAME_2 <- colnames(CROCHET_REPLACE_ENV$CUSTOM_OBJECT)[2]
    m <- matrix(data = c(ROW_NAME_1, COL_NAME_1, ROW_NAME_2, COL_NAME_2), ncol = 2, byrow = TRUE)
    test_replacement(m, value = value)
    m <- matrix(data = c(ROW_NAME_2, COL_NAME_2, ROW_NAME_1, COL_NAME_1), ncol = 2, byrow = TRUE)
    test_replacement(m, value = value)
}

# single replacement by NA
test_replacement(NA, value = value)
test_replacement(NA_integer_, value = value)
#test_replacement_not_implemented(NA_character_, value = value) # expansion behavior (tricky to implement)

# single replacement by zero
test_replacement_not_implemented(0, value = value)
test_replacement(c(0, 1), value = value)
test_replacement(c(0, -1), value = value)
test_replacement_error(c(0, 1, -1), value = value)

# single out-of-bounds replacements
if (!is.null(CROCHET_REPLACE_ENV$SKIP_OUT_OF_BOUNDS_TESTS) && CROCHET_REPLACE_ENV$SKIP_OUT_OF_BOUNDS_TESTS) {
    message("skipping out-of-bounds tests because tests were explicitly disabled")
} else {
    # positive integers
    test_replacement_not_implemented(CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT, value = value) # expansion behavior
    test_replacement_not_implemented(c(CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT, 2), value = value) # expansion behavior
    test_replacement_not_implemented(c(2, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), value = value) # expansion behavior
    m <- matrix(data = c(CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), ncol = 2, byrow = TRUE)
    test_replacement_error(m, value = value)
    # negative integers
    test_replacement(-CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT, value = value)
    test_replacement(c(-CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT, -2), value = value)
    test_replacement(c(-2, -CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), value = value)
    # logicals
    #test_replacement_not_implemented(rep_len(TRUE, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), value = value) # expansion behavior (tricky to implement)
    #test_replacement_not_implemented(rep_len(FALSE, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), value = value) # expansion behavior (tricky to implement)
    #test_replacement_not_implemented(rep_len(c(TRUE, FALSE), CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), value = value) # expansion behavior (tricky to implement)
    #test_replacement_not_implemented(rep_len(c(FALSE, TRUE), CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), value = value) # expansion behavior (tricky to implement)
    # characters
    m <- matrix(data = c(CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_CHAR, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_CHAR), ncol = 2, byrow = TRUE)
    test_replacement_error(m, value = value)
}

# multi replacement by nothing
test_replacement(, , value = value)

# multi replacement by positive integers
test_replacement(1, , value = value)
test_replacement(, 1, value = value)
test_replacement(1, 1, value = value)
test_replacement(c(1, 2), , value = value)
test_replacement(, c(1, 2), value = value)
test_replacement(c(1, 2), c(1, 2), value = value)
test_replacement(c(2, 1), , value = value)
test_replacement(, c(2, 1), value = value)
test_replacement(c(2, 1), c(2, 1), value = value)
test_replacement(1.1, , value = value)
test_replacement(, 1.1, value = value)
test_replacement(1.1, 1.1, value = value)
test_replacement(c(1.1, 2.1), , value = value)
test_replacement(, c(1.1, 2.1), value = value)
test_replacement(c(1.1, 2.1), c(1.1, 2.1), value = value)
test_replacement(c(2.1, 1.1), , value = value)
test_replacement(, c(2.1, 1.1), value = value)
test_replacement(c(2.1, 1.1), c(2.1, 1.1), value = value)
test_replacement(1.9, , value = value)
test_replacement(, 1.9, value = value)
test_replacement(1.9, 1.9, value = value)
test_replacement(c(1.9, 2.9), , value = value)
test_replacement(, c(1.9, 2.9), value = value)
test_replacement(c(1.9, 2.9), c(1.9, 2.9), value = value)
test_replacement(c(2.9, 1.9), , value = value)
test_replacement(, c(2.9, 1.9), value = value)
test_replacement(c(2.9, 1.9), c(2.9, 1.9), value = value)
test_replacement(c(1, NA), , value = value)
test_replacement(, c(1, NA), value = value)
test_replacement(c(1, NA), c(1, NA), value = value)

# multi replacement by negative integers
test_replacement(c(-1), , value = value)
test_replacement(, c(-1), value = value)
test_replacement(c(-1), c(-1), value = value)
test_replacement(c(-1, -2), , value = value)
test_replacement(, c(-1, -2), value = value)
test_replacement(c(-1, -2), c(-1, -2), value = value)
test_replacement(c(-2, -1), , value = value)
test_replacement(, c(-2, -1), value = value)
test_replacement(c(-2, -1), c(-2, -1), value = value)
test_replacement(c(-1.1), , value = value)
test_replacement(, c(-1.1), value = value)
test_replacement(c(-1.1), c(-1.1), value = value)
test_replacement(c(-1.1, -2.1), , value = value)
test_replacement(, c(-1.1, -2.1), value = value)
test_replacement(c(-1.1, -2.1), c(-1.1, -2.1), value = value)
test_replacement(c(-2.1, -1.1), , value = value)
test_replacement(, c(-2.1, -1.1), value = value)
test_replacement(c(-2.1, -1.1), c(-2.1, -1.1), value = value)
test_replacement(c(-1.9), , value = value)
test_replacement(, c(-1.9), value = value)
test_replacement(c(-1.9), c(-1.9), value = value)
test_replacement(c(-1.9, -2.9), , value = value)
test_replacement(, c(-1.9, -2.9), value = value)
test_replacement(c(-1.9, -2.9), c(-1.9, -2.9), value = value)
test_replacement(c(-2.9, -1.9), , value = value)
test_replacement(, c(-2.9, -1.9), value = value)
test_replacement(c(-2.9, -1.9), c(-2.9, -1.9), value = value)
test_replacement_error(c(-1, NA), , value = value)
test_replacement_error(, c(-1, NA), value = value)
test_replacement_error(c(-1, NA), c(-1, NA), value = value)

# multi replacement by logicals
test_replacement(c(TRUE), , value = value)
test_replacement(, c(TRUE), value = value)
test_replacement(c(TRUE), c(TRUE), value = value)
test_replacement(c(FALSE), , value = value)
test_replacement(, c(FALSE), value = value)
test_replacement(c(FALSE), c(FALSE), value = value)
test_replacement(c(TRUE, FALSE), , value = value)
test_replacement(, c(TRUE, FALSE), value = value)
test_replacement(c(TRUE, FALSE), c(TRUE, FALSE), value = value)
test_replacement(c(FALSE, TRUE), , value = value)
test_replacement(, c(FALSE, TRUE), value = value)
test_replacement(c(FALSE, TRUE), c(FALSE, TRUE), value = value)
test_replacement(c(TRUE, NA), , value = value)
test_replacement(, c(TRUE, NA), value = value)
test_replacement(c(TRUE, NA), c(TRUE, NA), value = value)
test_replacement(c(FALSE, NA), , value = value)
test_replacement(, c(FALSE, NA), value = value)
test_replacement(c(FALSE, NA), c(FALSE, NA), value = value)

# multi replacement by characters
if (is.null(dimnames(CROCHET_REPLACE_ENV$CUSTOM_OBJECT))) {
    message("skipping character replacement because dimnames are NULL")
} else {
    ROW_NAME_1 <- rownames(CROCHET_REPLACE_ENV$CUSTOM_OBJECT)[1]
    ROW_NAME_2 <- rownames(CROCHET_REPLACE_ENV$CUSTOM_OBJECT)[2]
    COL_NAME_1 <- colnames(CROCHET_REPLACE_ENV$CUSTOM_OBJECT)[1]
    COL_NAME_2 <- colnames(CROCHET_REPLACE_ENV$CUSTOM_OBJECT)[2]
    test_replacement(ROW_NAME_1, , value = value)
    test_replacement(, COL_NAME_1, value = value)
    test_replacement(ROW_NAME_1, COL_NAME_1, value = value)
    test_replacement(c(ROW_NAME_1, ROW_NAME_2), , value = value)
    test_replacement(, c(COL_NAME_1, COL_NAME_2), value = value)
    test_replacement(c(ROW_NAME_1, ROW_NAME_2), c(COL_NAME_1, COL_NAME_2), value = value)
    test_replacement(c(ROW_NAME_2, ROW_NAME_1), , value = value)
    test_replacement(, c(COL_NAME_2, COL_NAME_1), value = value)
    test_replacement(c(ROW_NAME_2, ROW_NAME_1), c(COL_NAME_2, COL_NAME_1), value = value)
    test_replacement_error(c(ROW_NAME_1, NA), , value = value)
    test_replacement_error(, c(COL_NAME_1, NA), value = value)
    test_replacement_error(c(ROW_NAME_1, NA), c(COL_NAME_1, NA), value = value)
}

# multi replacement by NA
test_replacement(NA, , value = value)
test_replacement(, NA, value = value)
test_replacement(NA, NA, value = value)
test_replacement(NA_integer_, , value = value)
test_replacement(, NA_integer_, value = value)
test_replacement(NA_integer_, NA_integer_, value = value)
test_replacement_error(NA_character_, , value = value)
test_replacement_error(, NA_character_, value = value)
test_replacement_error(NA_character_, NA_character_, value = value)

# multi replacement by zero
test_replacement_not_implemented(0, 0, value = value)
test_replacement(c(0, 1), , value = value)
test_replacement(, c(0, 1), value = value)
test_replacement(c(0, 1), c(0, 1), value = value)
test_replacement(c(0, -1), , value = value)
test_replacement(, c(0, -1), value = value)
test_replacement(c(0, -1), c(0, -1), value = value)
test_replacement_error(c(0, 1, -1), , value = value)
test_replacement_error(, c(0, 1, -1), value = value)
test_replacement_error(c(0, 1, -1), c(0, 1, -1), value = value)

# multi out-of-bounds replacements
if (!is.null(CROCHET_REPLACE_ENV$SKIP_OUT_OF_BOUNDS_TESTS) && CROCHET_REPLACE_ENV$SKIP_OUT_OF_BOUNDS_TESTS) {
    message("skipping out-of-bounds tests because tests were explicitly disabled")
} else {
    # positive integers
    test_replacement_error(CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT, , value = value)
    test_replacement_error(, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT, value = value)
    test_replacement_error(CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT, value = value)
    test_replacement_error(c(CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT, 2), , value = value)
    test_replacement_error(, c(CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT, 2), value = value)
    test_replacement_error(c(CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT, 2), c(CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT, 2), value = value)
    test_replacement_error(c(2, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), , value = value)
    test_replacement_error(, c(2, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), value = value)
    test_replacement_error(c(2, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), c(2, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), value = value)
    # negative integers
    test_replacement(c(-CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), , value = value)
    test_replacement(, c(-CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), value = value)
    test_replacement(c(-CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), c(-CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), value = value)
    test_replacement(c(-CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT, -2), , value = value)
    test_replacement(, c(-CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT, -2), value = value)
    test_replacement(c(-CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT, -2), c(-CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT, -2), value = value)
    test_replacement(c(-2, -CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), , value = value)
    test_replacement(, c(-2, -CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), value = value)
    test_replacement(c(-2, -CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), c(-2, -CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), value = value)
    # logicals
    test_replacement_error(c(rep_len(TRUE, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT)), , value = value)
    test_replacement_error(, c(rep_len(TRUE, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT)), value = value)
    test_replacement_error(c(rep_len(TRUE, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT)), c(rep_len(TRUE, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT)), value = value)
    test_replacement_error(c(rep_len(FALSE, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT)), , value = value)
    test_replacement_error(, c(rep_len(FALSE, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT)), value = value)
    test_replacement_error(c(rep_len(FALSE, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT)), c(rep_len(FALSE, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT)), value = value)
    test_replacement_error(rep_len(c(TRUE, FALSE), CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), , value = value)
    test_replacement_error(, rep_len(c(TRUE, FALSE), CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), value = value)
    test_replacement_error(rep_len(c(TRUE, FALSE), CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), rep_len(c(TRUE, FALSE), CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), value = value)
    test_replacement_error(rep_len(c(FALSE, TRUE), CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), , value = value)
    test_replacement_error(, rep_len(c(FALSE, TRUE), CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), value = value)
    test_replacement_error(rep_len(c(FALSE, TRUE), CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), rep_len(c(FALSE, TRUE), CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT), value = value)
    # character
    test_replacement_error(CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_CHAR, , value = value)
    test_replacement_error(, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_CHAR, value = value)
    test_replacement_error(CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_CHAR, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_CHAR, value = value)
    test_replacement_error(c(ROW_NAME_1, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_CHAR), , value = value)
    test_replacement_error(, c(COL_NAME_1, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_CHAR), value = value)
    test_replacement_error(c(ROW_NAME_1, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_CHAR), c(COL_NAME_1, CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_CHAR), value = value)
    test_replacement_error(c(CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_CHAR, ROW_NAME_1), , value = value)
    test_replacement_error(, c(CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_CHAR, COL_NAME_1), value = value)
    test_replacement_error(c(CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_CHAR, ROW_NAME_1), c(CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_CHAR, COL_NAME_1), value = value)
}

# n-dimensional replacement causes an error
test_replacement_error(1, 1, 1, value = value)

# value expansion
test_replacement(1, value = values[1])
test_replacement_warning(1, value = values[1:2])
test_replacement_warning(1, value = rep_len(values, length.out = length))
test_replacement_warning(1, value = rep_len(values, length.out = length + 3))
test_replacement(c(1, 2, 3), value = values[1])
test_replacement(c(1, 2, 3), value = values[1:3])
test_replacement_warning(c(1, 2, 3), value = values[1:2])
test_replacement_warning(c(1, 2, 3), value = rep_len(values, length.out = length))
test_replacement_warning(c(1, 2, 3), value = rep_len(values, length.out = length + 3))
