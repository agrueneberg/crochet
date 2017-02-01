# Subsette

An implementation of the extraction / subsetting / indexing function `[` for custom matrix-like types (based on S3, S4, etc.), modeled as closely to the base `matrix` class as possible (with tests to prove it).


## Example

```R
b <- matrix(data = rnorm(25), nrow = 5, ncol = 5)
dimnames(b) <- list(letters[1:5], letters[1:5])

a <- structure(list(), class = "TestMatrix")

dim.TestMatrix <- function(x) c(5, 5)

dimnames.TestMatrix <- function(x) dimnames(b)

`[.TestMatrix` <- subsette

vectorSubset <- function(x, i) {
    # Dispatch to b instead to x for this demo
    b[i, drop = FALSE]
}

matrixSubset <- function(x, i, j) {
    # Dispatch to b instead to x for this demo
    b[i, j, drop = FALSE]
}
```
