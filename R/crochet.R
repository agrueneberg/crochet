convertIndex <- function(x, i, type, allowDoubles = FALSE) {
    if (type == "k") {
        # Single Index
        n <- length(x)
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
            if (inherits(i, "matrix") && ncol(i) == 2L) {
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
    } else if (type == "k" && inherits(i, "matrix") && ncol(i) == 2L && is.numeric(i)) { # x[y > 1]
        i <- (as.integer(i[, 2L]) - 1L) * nrow(x) + as.integer(i[, 1L])
        checkBounds <- TRUE
    } else if (is.numeric(i)) { # x[1], x[-1], x[0]
        if (typeof(i) == "double") {
            # Convert to integer if there is no overflow
            if (!(allowDoubles && any(abs(i) > .Machine$integer.max, na.rm = TRUE))) {
                i <- as.integer(i)
            }
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

extract <- function(extract_vector, extract_matrix, allowDoubles = FALSE) {

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

        # The index type can be determined using a mixture of missing(i),
        # missing(j), and nargs(). missing(i) and missing(j) are enough to
        # differentiate x[] (as missing(i) && missing(j)) and x[i, j] (as
        # !missing(i) && !(missing(j)), but x[i] and x[i, ] are both
        # !missing(i) && missing(j). nargs() counts missing arguments and is 2L
        # in the case of x[i] and 3L in the case of x[i, j], but drop and the
        # optional named arguments in ... need to be subtracted from the count.

        dotdotdot <- list(...)
        numArgs <- nargs() - !missing(drop) - length(names(dotdotdot))

        # Single Index: x[i]
        if (numArgs == 2L && !missing(i) && missing(j)) {
            i <- convertIndex(x, i, "k", allowDoubles = allowDoubles)
            subset <- extract_vector(x, i, ...)
        # Multi Index: x[i, j], x[i, ], or x[, j]
        } else if (numArgs == 3L && (!missing(i) || !missing(j))) {
            if (missing(i)) {
                i <- seq(1L, nrow(x))
            } else {
                i <- convertIndex(x, i, "i", allowDoubles = allowDoubles)
            }
            if (missing(j)) {
                j <- seq(1L, ncol(x))
            } else {
                j <- convertIndex(x, j, "j", allowDoubles = allowDoubles)
            }
            subset <- extract_matrix(x, i, j, ...)
            # Let R handle drop behavior: as.vector removes names
            if (drop == TRUE && (nrow(subset) == 1L || ncol(subset) == 1L)) {
                subset <- subset[, , drop = TRUE]
            }
        # No Index: x[] or x[, ]
        } else if (missing(i) && missing(j)) {
            i <- seq(1L, nrow(x))
            j <- seq(1L, ncol(x))
            subset <- extract_matrix(x, i, j, ...)
        } else {
            stop("incorrect number of dimensions")
        }

        return(subset)

    })

}

replace <- function(replace_vector, replace_matrix, allowDoubles = FALSE) {

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

        dotdotdot <- list(...)
        numArgs <- nargs() - length(names(dotdotdot))

        # Single Index: x[i]
        if (numArgs == 3L && !missing(i) && missing(j)) {
            i <- convertIndex(x, i, "k", allowDoubles = allowDoubles)
            i <- handleNAs(i, value)
            if (any(i > length(x))) {
                stop("out-of-bounds expansion not implemented")
            }
            value <- expandValue(value, length(i))
            x <- replace_vector(x, i, ..., value = value)
        # Multi Index: x[i, j], x[i, ], or x[, j]
        } else if (numArgs == 4L && (!missing(i) || !missing(j))) {
            if (missing(i)) {
                i <- seq(1L, nrow(x))
            } else {
                i <- convertIndex(x, i, "i", allowDoubles = allowDoubles)
            }
            if (missing(j)) {
                j <- seq(1L, ncol(x))
            } else {
                j <- convertIndex(x, j, "j", allowDoubles = allowDoubles)
            }
            i <- handleNAs(i, value)
            j <- handleNAs(j, value)
            value <- expandValue(value, length(i) * length(j))
            x <- replace_matrix(x, i, j, ..., value = value)
        # No Index: x[] or x[, ]
        } else if (missing(i) && missing(j)) {
            i <- seq(1L, nrow(x))
            j <- seq(1L, ncol(x))
            value <- expandValue(value, length(i) * length(j))
            x <- replace_matrix(x, i, j, ..., value = value)
        } else {
            stop("incorrect number of subscripts")
        }

        return(x)

    })

}

# Incomplete check if x is matrix-like. Should check for length as well.
isMatrixLike <- function(x) {
    length(dim(x)) == 2L
}

ktoij <- function(x, k) {
    k <- k - 1L
    n <- nrow(x)
    i <- (k %% n) + 1L
    j <- floor(k / n) + 1L # use floor instead of as.integer to support doubles
    list(i = i, j = j)
}

ijtok <- function(x, i, j) {
    (j - 1L) * nrow(x) + i
}
