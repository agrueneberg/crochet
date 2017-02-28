b <- matrix(data = rnorm(25), nrow = 5, ncol = 5)
dimnames(b) <- list(letters[1:5], letters[1:5])

a <- structure(list(), class = "TestMatrix")

dim.TestMatrix <- function(x) c(5, 5)

dimnames.TestMatrix <- function(x) dimnames(b)

extract_vector <- function(x, i) {
        # Dispatch to b instead to x for this demo
        b[i, drop = FALSE]
}

extract_matrix <- function(x, i, j) {
        # Dispatch to b instead to x for this demo
        b[i, j, drop = FALSE]
}

`[.TestMatrix` <- extract(extract_vector = extract_vector, extract_matrix = extract_matrix)
