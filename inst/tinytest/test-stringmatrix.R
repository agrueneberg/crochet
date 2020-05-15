# Define StringMatrix, an example of a custom type that implements extraction
# using the 'extract' function and replacement using the 'replace' function.
# See README for more information on this example type.

registerS3method("length", "StringMatrix", function(x) {
    prod(attr(x, "_dim"))
})

registerS3method("dim", "StringMatrix", function(x) {
    attr(x, "_dim")
})

registerS3method("dimnames", "StringMatrix", function(x) {
    attr(x, "_dimnames")
})

registerS3method("[", "StringMatrix", extract(
    extract_vector = function(x, i, ...) {
        Z <- vector(mode = typeof(attr(x, "_data")), length = length(i))
        # Handle x[FALSE]
        if (length(i) > 0L) {
            for (idx in 1L:length(i)) {
                if (is.na(i[idx])) {
                    # Handle NAs
                    value <- NA_character_
                } else if (i[idx] > nchar(attr(x, "_data"))) {
                    # Handle out of bounds
                    value <- NA_character_
                } else {
                    value <- substr(attr(x, "_data"), i[idx], i[idx])
                }
                Z[idx] <- value
            }
        }
        return(Z)
    },
    extract_matrix = function(x, i, j, ...) {
        Z <- matrix(
            data = vector(mode = typeof(attr(x, "_data")), length = length(i) * length(j)),
            nrow = length(i),
            ncol = length(j),
            dimnames = list(attr(x, "_dimnames")[[1L]][i], attr(x, "_dimnames")[[2L]][j])
        )
        # Handle x[FALSE, ] or x[, FALSE]
        if (length(i) > 0L && length(j) > 0L) {
            for (colIdx in 1L:length(j)) {
                for (rowIdx in 1L:length(i)) {
                    k <- ijtok(x, i[rowIdx], j[colIdx])
                    Z[rowIdx, colIdx] <- substr(attr(x, "_data"), k, k)
                }
            }
        }
        return(Z)
    }
))

registerS3method("[<-", "StringMatrix", replace(
    replace_vector = function(x, i, ..., value) {
        value <- as.character(value)
        # Handle x[FALSE]
        if (length(i) > 0L) {
            for (idx in 1L:length(i)) {
                substr(attr(x, "_data"), i[idx], i[idx])  <- value[idx]
            }
        }
        return(x)
    }, replace_matrix = function(x, i, j, ..., value) {
        value <- as.character(value)
        dim(value) <- c(length(i), length(j))
        # Handle x[FALSE, ] or x[, FALSE]
        if (length(i) > 0L && length(j) > 0L) {
            for (colIdx in 1L:length(j)) {
                for (rowIdx in 1L:length(i)) {
                    k <- ijtok(x, i[rowIdx], j[colIdx])
                    substr(attr(x, "_data"), k, k) <- value[rowIdx, colIdx]
                }
            }
        }
        return(x)
    }
))

# Prepare instances of custom type for testing
seed <- 4711L
n <- 5L
p <- 5L
dimnames <- list(paste0("row_", 1L:n), paste0("col_", 1L:p))
VALUE_POOL <- c(0:9, letters)
OUT_OF_BOUNDS_INT <- 100
OUT_OF_BOUNDS_CHAR <- "x"

generateValues <- function() {
    set.seed(seed)
    sample(VALUE_POOL, replace = TRUE, size = n * p)
}

createStringMatrix <- function() {
    obj <- list()
    class(obj) <- "StringMatrix"
    attr(obj, "_dim") <- c(n, p)
    attr(obj, "_dimnames") <- dimnames
    attr(obj, "_data") <- paste(generateValues(), collapse = "")
    return(obj)
}

createMatrix <- function() {
    matrix(data = generateValues(), nrow = n, ncol = p, dimnames = dimnames)
}

# Source extraction tests
extractionTests <- new.env()
extractionTests$OUT_OF_BOUNDS_INT <- OUT_OF_BOUNDS_INT
extractionTests$OUT_OF_BOUNDS_CHAR <- OUT_OF_BOUNDS_CHAR
extractionTests$COMPARE_OBJECT <- createMatrix()
extractionTests$CUSTOM_OBJECT <- createStringMatrix()
source(
    file = system.file("test-suite", "crochet-extract.R", package = "crochet"),
    local = extractionTests
)

# Source replacement tests
replacementTests <- new.env()
replacementTests$OUT_OF_BOUNDS_INT <- OUT_OF_BOUNDS_INT
replacementTests$OUT_OF_BOUNDS_CHAR <- OUT_OF_BOUNDS_CHAR
replacementTests$VALUE_POOL <- VALUE_POOL
replacementTests$RESET <- function() {
    replacementTests$COMPARE_OBJECT <- createMatrix()
    replacementTests$CUSTOM_OBJECT <- createStringMatrix()
}
replacementTests$RESET()
source(
    file = system.file("test-suite", "crochet-replace.R", package = "crochet"),
    local = replacementTests
)
