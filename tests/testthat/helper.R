createMatrix <- function() {
    set.seed(4711)
    m <- matrix(data = rnorm(25), nrow = 5, ncol = 5)
    dimnames(m) <- list(letters[1:5], letters[6:10])
    return(m)
}

OUT_OF_BOUNDS_INT <- 100
OUT_OF_BOUNDS_CHAR <- "x"

# extract

CROCHET_EXTRACT_ENV <- new.env()

CROCHET_EXTRACT_ENV$OUT_OF_BOUNDS_INT <- OUT_OF_BOUNDS_INT
CROCHET_EXTRACT_ENV$OUT_OF_BOUNDS_CHAR <- OUT_OF_BOUNDS_CHAR

CROCHET_EXTRACT_ENV$COMPARE_OBJECT <- createMatrix()

CROCHET_EXTRACT_ENV$CUSTOM_OBJECT <- structure(list(), class = "CustomExtractMatrix")

registerS3method("dim", "CustomExtractMatrix", function(x) {
    # Dispatch to COMPARE_OBJECT instead of x
    dim(CROCHET_EXTRACT_ENV$COMPARE_OBJECT)
})

registerS3method("dimnames", "CustomExtractMatrix", function(x) {
    # Dispatch to COMPARE_OBJECT instead of x
    dimnames(CROCHET_EXTRACT_ENV$COMPARE_OBJECT)
})

extract_vector <- function(x, i) {
    # Dispatch to COMPARE_OBJECT instead of x
    CROCHET_EXTRACT_ENV$COMPARE_OBJECT[i, drop = FALSE]
}

extract_matrix <- function(x, i, j) {
    # Dispatch to COMPARE_OBJECT instead of x
    CROCHET_EXTRACT_ENV$COMPARE_OBJECT[i, j, drop = FALSE]
}

registerS3method("[", "CustomExtractMatrix", extract(extract_vector = extract_vector, extract_matrix = extract_matrix))

# replace

CROCHET_REPLACE_ENV <- new.env()

CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT <- OUT_OF_BOUNDS_INT
CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_CHAR <- OUT_OF_BOUNDS_CHAR
CROCHET_REPLACE_ENV$VALUE_POOL <- as.double(0:9)

CROCHET_REPLACE_ENV$RESET <- function() {
    CROCHET_REPLACE_ENV$COMPARE_OBJECT <- createMatrix()
    CROCHET_REPLACE_ENV$backend <- createMatrix()
}

CROCHET_REPLACE_ENV$RESET()

CROCHET_REPLACE_ENV$CUSTOM_OBJECT <- structure(list(), class = "CustomReplaceMatrix")

registerS3method("dim", "CustomReplaceMatrix", function(x) {
    # Dispatch to backend instead of x
    dim(CROCHET_REPLACE_ENV$backend)
})

registerS3method("dimnames", "CustomReplaceMatrix", function(x) {
    # Dispatch to backend instead of x
    dimnames(CROCHET_REPLACE_ENV$backend)
})

registerS3method("[", "CustomReplaceMatrix", function(x, ...) {
    # Dispatch to backend instead of x
    CROCHET_REPLACE_ENV$backend[...]
})

replace_vector <- function(x, i, value) {
    CROCHET_REPLACE_ENV$i <- i
    CROCHET_REPLACE_ENV$value <- value
    # Dispatch to backend instead of x
    with(CROCHET_REPLACE_ENV, backend[i] <- value)
    return(x)
}

replace_matrix <- function(x, i, j, value) {
    CROCHET_REPLACE_ENV$i <- i
    CROCHET_REPLACE_ENV$j <- j
    CROCHET_REPLACE_ENV$value <- value
    # Dispatch to backend instead of x
    with(CROCHET_REPLACE_ENV, backend[i, j] <- value)
    return(x)
}

registerS3method("[<-", "CustomReplaceMatrix", replace(replace_vector = replace_vector, replace_matrix = replace_matrix))
