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
            i <- i[1L:len_initial_i_sans_false]
        }
    } else if (typeof(i) == "character") { # x["a"]
        if (type == "k") {
            if (class(i) == "matrix" && ncol(i) == 2L) {
                i <- (match(i[, 2L], colnames(x)) - 1L) * nrow(x) + match(i[, 1L], rownames(x))
                if (any(is.na(i))) {
                    stop("subscript out of bounds")
                }
            } else {
                i <- rep(NA_integer_, times = length(i))
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
        i <- (as.integer(i[, 2L]) - 1L) * nrow(x) + as.integer(i[, 1L])
        checkBounds <- TRUE
    } else if (is.numeric(i)) { # x[1], x[-1], x[0]
        if (typeof(i) == "double") {
            i <- as.integer(i)
        }
        # Remove all 0s
        i <- i[i != 0L]
        if (length(i) == 0L) { # x[0]
            stop("indexing by 0 not implemented")
        } else {
            i_is_na <- is.na(i)
            # Negative integers are not allowed to be combined with
            # positive integers or missing values
            if (any(i < 0L, na.rm = TRUE) && (any(i > 0L, na.rm = TRUE) || any(i_is_na))) {
                stop("only 0's may be mixed with negative subscripts")
            } else if (all(i < 0L, na.rm = TRUE)) { # x[-1]
                i <- seq(1L, n)[i]
            }
        }
    }
    if (checkBounds && any(i > n, na.rm = TRUE)) {
        stop("subscript out of bounds")
    }
    return(i)
}

handleNAs <- function(i, value) {
    isNA <- is.na(i)
    if (any(isNA)) {
        if (length(value) == 1L) {
            i <- i[!isNA]
        } else {
            stop("NAs are not allowed in subscripted assignments")
        }
    }
    return(i)
}

expandValue <- function(value, replacement_length) {
    value_length <- length(value)
    if (value_length == 0L) {
        stop("replacement has length zero")
    } else if (value_length != replacement_length) {
        if (value_length != 1L) {
            warning("number of items to replace is not a multiple of replacement length")
        }
        value <- rep_len(value, length.out = replacement_length)
    }
    return(value)
}

#' Create an Implementation of [ For Custom Matrix-Like Types
#'
#' `extract` is a function that converts different index types such as negative
#' integer vectors, character vectors, or logical vectors passed to the `[`
#' function as `i` (e.g. `X[i]`) or `i` and `j` (e.g. `X[i, j]`) into positive
#' integer vectors. The converted indices are provided as the `i` parameter of
#' `extract_vector` or `i` and `j` parameters of `extract_matrix` to facilitate
#' implementing the extraction mechanism for custom matrix-like types.
#'
#' The custom type must implement methods for [base::dim()] and
#' [base::dimnames()] for this function to work. Implementing methods for
#' [base::nrow()], [base::ncol()], [base::rownames()], and [base::colnames()]
#' is not necessary as the default method of those generics calls [base::dim()]
#' or [base::dimnames()] internally.
#'
#' @param extract_vector A function in the form of `function(x, i, ...)` that
#' takes a subset of `x` based on a single index `i` and returns a vector.
#' @param extract_matrix A function in the form of `function(x, i, j, ...)`
#' that takes a subset of `x` based on two indices `i` and `j` and returns a
#' matrix.
#' @return A function in the form of `function(x, i, j, ..., drop = TRUE)` that
#' is meant to be used as a method for \code{\link[base]{[}} for a custom type.
#' @example man/examples/extract.R
#' @export
extract <- function(extract_vector, extract_matrix) {

    if (missing(extract_vector) || typeof(extract_vector) != "closure") {
        stop("extract_vector has to be of type closure")
    }
    extract_vector_formals <- methods::formalArgs(extract_vector)
    if (is.null(extract_vector_formals) || length(extract_vector_formals) < 2L || extract_vector_formals[1L] != "x" || extract_vector_formals[2L] != "i") {
        stop("extract_vector requires two arguments x and i")
    }

    if (missing(extract_matrix) || typeof(extract_matrix) != "closure") {
        stop("extract_matrix has to be of type closure")
    }
    extract_matrix_formals <- methods::formalArgs(extract_matrix)
    if (is.null(extract_matrix_formals) || length(extract_matrix_formals) < 3L || extract_matrix_formals[1L] != "x" || extract_matrix_formals[2L] != "i" || extract_matrix_formals[3L] != "j") {
        stop("extract_matrix requires three arguments x, i, and j")
    }

    return(function(x, i, j, ..., drop = TRUE) {

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
            subset <- extract_vector(x, i)
        # Multi Index: x[i, j], x[i, ], or x[, j]
        } else if (nargs == 3L && (!missing(i) || !missing(j))) {
            if (missing(i)) {
                i <- seq(1L, nrow(x))
            } else {
                i <- convertIndex(x, i, "i")
            }
            if (missing(j)) {
                j <- seq(1L, ncol(x))
            } else {
                j <- convertIndex(x, j, "j")
            }
            subset <- extract_matrix(x, i, j, ...)
            # Let R handle drop behavior: as.vector removes names
            if (drop == TRUE && (nrow(subset) == 1L || ncol(subset) == 1L)) {
                subset <- subset[, , drop = TRUE]
            }
        # No Index: x[] or x[, ]
        } else {
            i <- seq(1L, nrow(x))
            j <- seq(1L, ncol(x))
            subset <- extract_matrix(x, i, j, ...)
        }

        return(subset)

    })

}

#' Create an Implementation of [<- For Custom Matrix-Like Types
#'
#' `replace` is a function that converts different index types such as negative
#' integer vectors, character vectors, or logical vectors passed to the `[<-`
#' function as `i` (e.g. `X[i]`) or `i` and `j` (e.g. `X[i, j]`) into positive
#' integer vectors. The converted indices are provided as the `i` parameter of
#' `replace_vector` or `i` and `j` parameters of `replace_matrix` to facilitate
#' implementing the replacement mechanism for custom matrix-like types. Values
#' are recycled to match the replacement length.
#'
#' The custom type must implement methods for [base::dim()] and
#' [base::dimnames()] for this function to work. Implementing methods for
#' [base::nrow()], [base::ncol()], [base::rownames()], and [base::colnames()]
#' is not necessary as the default method of those generics calls [base::dim()]
#' or [base::dimnames()] internally.
#'
#' @param replace_vector A function in the form of `function(x, i, ..., value)`
#' that replaces a vector subset of `x` based on a single index `i` with the
#' values in `value` and returns `x`.
#' @param replace_matrix A function in the form of `function(x, i, j, ...,
#' value)` that replaces a matrix subset of `x` based on two indices `i` and
#' `j` with the values in `value` and returns `x`.
#' @return A function in the form of `function(x, i, j, ..., value)` that is
#' meant to be used as a method for \code{\link[base]{[<-}} for a custom type.
#' @example man/examples/replace.R
#' @export
replace <- function(replace_vector, replace_matrix) {

    if (missing(replace_vector) || typeof(replace_vector) != "closure") {
        stop("replace_vector has to be of type closure")
    }
    replace_vector_formals <- methods::formalArgs(replace_vector)
    if (is.null(replace_vector_formals) || length(replace_vector_formals) < 2L || replace_vector_formals[1L] != "x" || replace_vector_formals[2L] != "i" || !("value" %in% replace_vector_formals)) {
        stop("replace_vector requires three arguments x, i, and value")
    }

    if (missing(replace_matrix) || typeof(replace_matrix) != "closure") {
        stop("replace_matrix has to be of type closure")
    }
    replace_matrix_formals <- methods::formalArgs(replace_matrix)
    if (is.null(replace_matrix_formals) || length(replace_matrix_formals) < 3L || replace_matrix_formals[1L] != "x" || replace_matrix_formals[2L] != "i" || replace_matrix_formals[3L] != "j" || !("value" %in% replace_vector_formals)) {
        stop("replace_matrix requires four arguments x, i, j, and value")
    }

    return(function(x, i, j, ..., value) {

        nargs <- nargs()

        # Single Index: x[i]
        if (nargs == 3L && !missing(i) && missing(j)) {
            i <- convertIndex(x, i, "k")
            i <- handleNAs(i, value)
            if (any(i > prod(dim(x)))) {
                stop("out-of-bounds expansion not implemented")
            }
            value <- expandValue(value, length(i))
            x <- replace_vector(x, i, ..., value = value)
        # Multi Index: x[i, j], x[i, ], or x[, j]
        } else if (nargs == 4L && (!missing(i) || !missing(j))) {
            if (missing(i)) {
                i <- seq(1L, nrow(x))
            } else {
                i <- convertIndex(x, i, "i")
            }
            if (missing(j)) {
                j <- seq(1L, ncol(x))
            } else {
                j <- convertIndex(x, j, "j")
            }
            i <- handleNAs(i, value)
            j <- handleNAs(j, value)
            value <- expandValue(value, length(i) * length(j))
            x <- replace_matrix(x, i, j, ..., value = value)
        # No Index: x[] or x[, ]
        } else {
            i <- seq(1L, nrow(x))
            j <- seq(1L, ncol(x))
            value <- expandValue(value, length(i) * length(j))
            x <- replace_matrix(x, i, j, ..., value = value)
        }

        return(x)

    })

}

# Convert one-dimensional index k to two-dimensional indices i and j.
# Assumes indices to be one-based.
ktoij <- function(x, k) {
    k <- k - 1L
    n <- nrow(x)
    i <- (k %% n) + 1L
    j <- as.integer(k / n) + 1L
    list(i = i, j = j)
}

# Convert two-dimensional indices i and j to one-dimensional index k.
# Assumes indices to be one-based.
ijtok <- function(x, i, j) {
    (j - 1L) * nrow(x) + i
}
