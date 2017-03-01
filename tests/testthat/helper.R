CROCHET_EXTRACT_B <- matrix(data = rnorm(25), nrow = 5, ncol = 5)
dimnames(CROCHET_EXTRACT_B) <- list(letters[1:5], letters[6:10])

extract_vector <- function(x, i) {
    # Dispatch to CROCHET_EXTRACT_B instead to x
    CROCHET_EXTRACT_B[i, drop = FALSE]
}

extract_matrix <- function(x, i, j) {
    # Dispatch to CROCHET_EXTRACT_B instead to x
    CROCHET_EXTRACT_B[i, j, drop = FALSE]
}

CROCHET_EXTRACT_A <- structure(list(), class = "TestMatrix")

registerS3method("dim", "TestMatrix", function(x) dim(CROCHET_EXTRACT_B))

registerS3method("dimnames", "TestMatrix", function(x) dimnames(CROCHET_EXTRACT_B))

registerS3method("[", "TestMatrix", extract(extract_vector = extract_vector, extract_matrix = extract_matrix))

OUT_OF_BOUNDS_INT <- 100
OUT_OF_BOUNDS_CHAR <- "x"
