b <- matrix(data = rnorm(25), nrow = 5, ncol = 5)
dimnames(b) <- list(letters[1:5], letters[1:5])

a <- structure(list(), class = "TestMatrix")

dim.TestMatrix <- function(x) c(5, 5)

dimnames.TestMatrix <- function(x) dimnames(b)

subset_vector <- function(x, i) {
        # Dispatch to b instead to x for this demo
        b[i, drop = FALSE]
}

subset_matrix <- function(x, i, j) {
        # Dispatch to b instead to x for this demo
        b[i, j, drop = FALSE]
}

`[.TestMatrix` <- extract(subset_vector = subset_vector, subset_matrix = subset_matrix)
