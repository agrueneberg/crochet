TST_B <- matrix(data = rnorm(25), nrow = 5, ncol = 5)
dimnames(TST_B) <- list(letters[1:5], letters[1:5])

subset_vector <- function(x, i) {
    # Dispatch to TST_B instead to x
    TST_B[i, drop = FALSE]
}

subset_matrix <- function(x, i, j) {
    # Dispatch to TST_B instead to x
    TST_B[i, j, drop = FALSE]
}

TST_A <- structure(list(), class = "TestMatrix")

registerS3method("dim", "TestMatrix", function(x) dim(TST_B))

registerS3method("dimnames", "TestMatrix", function(x) dimnames(TST_B))

registerS3method("[", "TestMatrix", subsette(subset_vector = subset_vector, subset_matrix = subset_matrix))

OUT_OF_BOUNDS <- 100
