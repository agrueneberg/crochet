b <- matrix(data = rnorm(25), nrow = 5, ncol = 5)
dimnames(b) <- list(letters[1:5], letters[1:5])

subset_vector <- function(x, i) {
    # Dispatch to b instead to x
    b[i, drop = FALSE]
}

subset_matrix <- function(x, i, j) {
    # Dispatch to b instead to x
    b[i, j, drop = FALSE]
}

a <- structure(list(), class = "TestMatrix")

registerS3method("dim", "TestMatrix", function(x) dim(b))

registerS3method("dimnames", "TestMatrix", function(x) dimnames(b))

registerS3method("[", "TestMatrix", subsette(subset_vector = subset_vector, subset_matrix = subset_matrix))
