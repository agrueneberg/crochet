# Subsette

[![Travis-CI Build Status](https://travis-ci.org/agrueneberg/subsette.svg?branch=master)](https://travis-ci.org/agrueneberg/subsette)

An implementation of the extraction / subsetting / indexing function `[` for custom matrix-like types (based on S3, S4, etc.), modeled as closely to the base `matrix` class as possible (with tests to prove it).


## Example

`subsette` is a function that accepts two arguments `subset_vector` (in the form of `function(x, i)`) and `subset_matrix` (in the form of `function(x, i, j)`), and returns a function that can be used as a method for `[` for a custom type.

The following example creates a dummy matrix `b` and an instance `a` of a custom type called `TestMatrix`. `TestMatrix` is an S3 "class" that in addition to the `[` implements methods for `dim` and `dimnames`. In this case, the `subset_vector` and `subset_matrix` function close over `b` and simply delegate the subsetting. Note that the `[` character is not allowed in a variable name, so it needs to be escaped with backticks.

```R
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

`[.TestMatrix` <- subsette(subset_vector = subset_vector, subset_matrix = subset_matrix)

b[1, ] # Get the subset from the source
a[1, ] # Get the subset through the subsette function
```
