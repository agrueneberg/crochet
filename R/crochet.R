#' Creates an implementation of [ for custom matrix-like types
#'
#' extract is a function that accepts two arguments `subset_vector` (in the
#' form of `function(x, i)`) and `subset_matrix` (in the form of `function(x,
#' i, j)`), and returns a function that can be used as a method for
#' \code{\link[base]{[}} for a custom type.
#'
#' The custom type must implement methods for [base::dim()] and
#' [base::dimnames()] for this function to work. Implementing methods for
#' [base::nrow()], [base::ncol()], [base::rownames()], and [base::colnames()]
#' is not necessary as the default method of those generics calls [base::dim()]
#' or [base::dimnames()] internally.
#'
#' @param subset_vector A function in the form of `function(x, i)` that takes a
#' subset of `x` based on a single index `i` and returns a vector.
#' @param subset_matrix A function in the form of `function(x, i, j)` that
#' takes a subset of `x` based on two indices `i` and `j` and returns a matrix.
#' @return A function in the form of `function(x, i, j, drop = TRUE` that is
#' meant to be used as a method for \code{\link[base]{[}}.
#' @export
#' @example inst/examples/extract.R
extract <- function(subset_vector, subset_matrix) {

    if (missing(subset_vector) || typeof(subset_vector) != "closure") {
        stop("subset_vector has to be of type closure")
    }
    subset_vector_formals <- methods::formalArgs(subset_vector)
    if (is.null(subset_vector_formals) || length(subset_vector_formals) < 2L || subset_vector_formals[1] != "x" || subset_vector_formals[2] != "i") {
        stop("subset_vector requires two arguments x and i")
    }

    if (missing(subset_matrix) || typeof(subset_matrix) != "closure") {
        stop("subset_matrix has to be of type closure")
    }
    subset_matrix_formals <- methods::formalArgs(subset_matrix)
    if (is.null(subset_matrix_formals) || length(subset_matrix_formals) < 3L || subset_matrix_formals[1] != "x" || subset_matrix_formals[2] != "i" || subset_matrix_formals[3] != "j") {
        stop("subset_matrix requires three arguments x, i, and j")
    }

    return(function(x, i, j, ..., drop = TRUE) {

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
                i <- (as.integer(i[, 2]) - 1) * nrow(x) + as.integer(i[, 1])
                checkBounds <- TRUE
            } else if (is.numeric(i)) { # x[1], x[-1], x[0]
                if (typeof(i) == "double") {
                    i <- as.integer(i)
                }
                # Remove all 0s
                i <- i[i != 0L]
                if (length(i) == 0L) { # x[0]
                    stop("Unsupported index type")
                } else {
                    i_is_na <- is.na(i)
                    # Negative integers are not allowed to be combined with
                    # positive integers or missing values
                    if (any(i < 0L, na.rm = TRUE) && (any(i > 0L, na.rm = TRUE) || any(i_is_na))) {
                        stop("only 0's may be mixed with negative subscripts")
                    } else if (all(i < 0L, na.rm = TRUE)) { # x[-1]
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
            subset <- subset_vector(x, i)
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
            subset <- subset_matrix(x, i, j, ...)
            # Let R handle drop behavior
            if (drop == TRUE && (nrow(subset) == 1L || ncol(subset) == 1L)) {
                subset <- subset[, ]
            }
        # No Index: x[] or x[, ]
        } else {
            i <- seq(1, nrow(x))
            j <- seq(1, ncol(x))
            subset <- subset_matrix(x, i, j, ...)
        }

        return(subset)

    })

}
