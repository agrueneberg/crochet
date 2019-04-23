# crochet

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/crochet)](https://CRAN.R-project.org/package=crochet)
[![Rdoc](http://www.rdocumentation.org/badges/version/crochet)](http://www.rdocumentation.org/packages/crochet)
[![Travis-CI Build Status](https://travis-ci.org/agrueneberg/crochet.svg?branch=master)](https://travis-ci.org/agrueneberg/crochet)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/5osfclaxqxhq04r2?svg=true)](https://ci.appveyor.com/project/agrueneberg/crochet)
[![Coverage status](https://codecov.io/gh/agrueneberg/crochet/branch/master/graph/badge.svg)](https://codecov.io/github/agrueneberg/crochet?branch=master)

crochet is an R package that provides functions to help implement the extraction / subsetting / indexing function `[` and replacement function `[<-` of custom matrix-like types (based on S3, S4, etc.), modeled as closely to the base `matrix` class as possible (with tests to prove it).


## Example

In this example we are going to implement a custom matrix-like type that stores data in form of characters as a string in the `_data` attribute of an object. This is not going to be particularly useful, but serves as an easy to understand starting point that can be adapted for other storage mechanisms, e.g., databases, shared memory, and so on.

First we need to clarify what we mean by matrix-like type: a matrix-like type is a class (i.e., implemented using any of R's object-oriented mechanisms) that implements **at least** the `dim()`, `length()`, `dimnames()`, and the `[` extraction method.

Let's call our type `StringMatrix` and implement it as an S3 class. If you need a refresher on S3 classes, please read the [OO field guide](http://adv-r.had.co.nz/OO-essentials.html) chapter in the **Advanced R** book by Hadley Wickham first. Let's start with implementing the `dim()`, `length()`, and `dimnames()` methods:

```R
dim.StringMatrix <- function(x) {
    attr(x, "_dim") # store dimensions in `_dim` attribute
}

length.StringMatrix <- function(x) {
    prod(dim(x)) # rely on `dim()` method above
}

dimnames.StringMatrix <- function(x) {
    attr(x, "_dimnames") # store dim names in `_dimnames` attribute
}
```

Subsetting in R is very powerful and can therefore be difficult to implement depending on how many indexing mechanisms you want to support. For example, given a 5x5 matrix, all of the following cases (non-exhaustive) are equivalent:

```R
X[seq(1, 25, by = 5)] # subsetting by positive integers
X[1, ] # simplifying subsetting by positive integers
X[-(2:5), ] # simplifying subsetting by negative integers
X[c(TRUE, FALSE, FALSE, FALSE, FALSE), ] # simplifying subsetting by booleans
X["row_1", ] # simplifying subsetting by row names (only if dimnames exist)
```

This complexity motivated the development of the crochet package. The `extract()` function of the package takes care of converting all those indexing mechanisms to positive integers, which is typically the easiest mechanism to implement. `extract()` returns a function that can be used as a method for `[` for a custom type. Matrices can be subsetted using one-dimensional (`i` only) and two-dimensional indices (`i` and `j`) and both have very different behaviors. Therefore, two functions need to be provided to `extract()` as `extract_vector` and `extract_matrix`. `extract_vector` has to be a function of the form `function(x, i, ...)` and `extract_matrix` a function of the form `function(x, i, j, ...)`. Both functions return a subset of `x`.

The following snippets gives a simple way to extract characters from a string one by one. In R, we can extract the *n*th character from a string using the `substr()` function: `substr(x, n, n)`. Conversely, the *n*th character can be replaced as follows: `substr(x, n, n) <- value`. Note that the `[` character is not allowed in a variable name, so it needs to be escaped with backticks when establishing the return value of `extract()` as a method of `StringMatrix`.

```R
`[.StringMatrix` <- extract(
    extract_vector = function(x, i, ...) { # i are positive integers
        # Reserve output vector
        subset <- vector(mode = "character", length = length(i))
        # Populate output vector
        for (singleIdx in 1:length(i)) {
            subset[singleIdx] <- substr(attr(x, "_data"), i[singleIdx], i[singleIdx])
        }
        # Return output vector
        return(subset)
    },
    extract_matrix = function(x, i, j, ...) { # i and j are positive integers
        # Reserve output matrix
        subset <- matrix(
            data = vector(mode = "character", length = length(i) * length(j)),
            nrow = length(i),
            ncol = length(j)
        )
        # Populate output matrix
        for (colIdx in 1:length(j)) {
            for (rowIdx in 1:length(i)) {
                # two-dimensional index needs to be converted to one-dimensional index
                singleIdx <- crochet:::ijtok(x, i[rowIdx], j[colIdx])
                subset[rowIdx, colIdx] <- substr(attr(x, "_data"), singleIdx, singleIdx)
            }
        }
        # Return output matrix
        return(subset)
    }
)
```

We can now create an object of the `StringMatrix` class and provide it with some data:

```R
# Generate data
n <- 5
p <- 5
alphabet <- c(0:9, letters)
data <- sample(alphabet, replace = TRUE, size = n * p)

# Create object
obj <- list()
class(obj) <- "StringMatrix"
attr(obj, "_dim") <- c(n, p)
attr(obj, "_dimnames") <- list(paste0("row_", 1:n), paste0("col_", 1:p))
attr(obj, "_data") <- paste(data, collapse = "")

# Call some methods
dim(obj)
nrow(obj) # you get this for free by implementing `dim()`
ncol(obj) # you get this for free by implementing `dim()`
length(obj)
dimnames(obj)
rownames(obj) # you get this for free by implementing `dimnames()`
colnames(obj) # you get this for free by implementing `dimnames()`

# Extract some data
obj[seq(1, length(obj), by = p)] # subsetting by positive integers
obj[1, ] # simplifying subsetting by positive integers
obj[-(2:length(obj)), ] # simplifying subsetting by negative integers
obj[c(TRUE, rep_len(FALSE, nrow(obj) - 1)), ] # simplifying subsetting by booleans
obj["row_1", ] # simplifying subsetting by row names (only if dimnames exist)
```

Different from `[` for atomic vectors (where both named and unnamed arguments are interpreted as indices), optional named arguments can be passed to `extract_vector` and `extact_matrix` as `...`. This can be useful for some optimization strategies (e.g., fadvise or madvise). Let's add an option to capitalize subsets as a demonstration:

```R
`[.StringMatrix` <- extract(
    extract_vector = function(x, i, ...) { # i are positive integers
        dotdotdot <- list(...)
        # Reserve output vector
        subset <- vector(mode = "character", length = length(i))
        # Populate output vector
        for (singleIdx in 1:length(i)) {
            subset[singleIdx] <- substr(attr(x, "_data"), i[singleIdx], i[singleIdx])
        }
        # Capitalize output
        if (!is.null(dotdotdot$capitalize) && dotdotdot$capitalize) {
            subset <- toupper(subset)
        }
        # Return output vector
        return(subset)
    },
    extract_matrix = function(x, i, j, ...) { # i and j are positive integers
        dotdotdot <- list(...)
        # Reserve output matrix
        subset <- matrix(
            data = vector(mode = "character", length = length(i) * length(j)),
            nrow = length(i),
            ncol = length(j)
        )
        # Populate output matrix
        for (colIdx in 1:length(j)) {
            for (rowIdx in 1:length(i)) {
                # two-dimensional index needs to be converted to one-dimensional index
                singleIdx <- crochet:::ijtok(x, i[rowIdx], j[colIdx])
                subset[rowIdx, colIdx] <- substr(attr(x, "_data"), singleIdx, singleIdx)
            }
        }
        # Capitalize output
        if (!is.null(dotdotdot$capitalize) && dotdotdot$capitalize) {
            subset <- toupper(subset)
        }
        # Return output matrix
        return(subset)
    }
)
```

Now we can capitalize the output as follows:

```R
obj[1, ]
obj[1, , capitalize = TRUE)
obj[1, , capitalize = FALSE)
```

To support replacement, `replace()` returns a function that can be used as a method for `[<-` for a custom type. Analogous to the `extract()` method, two parameters are required by `replace()`: `replace_vector` has to be a function of the form `function(x, i, ..., value)` and `replace_matrix` a function of the form `function(x, i, j, ..., value)`. Both functions return a likely modified version of `x`.

```R
`[<-.StringMatrix` <- replace(
    replace_vector = function(x, i, ..., value) { # i are positive integers
        # Perform replacement
        for (singleIdx in 1:length(i)) {
            substr(attr(x, "_data"), i[singleIdx], i[singleIdx]) <- value[singleIdx]
        }
        # Do not forget to return x
        return(x)
    },
    replace_matrix = function(x, i, j, ..., value) { # i and j are positive integers
        # Convert value to matrix for easier indexing
        dim(value) <- c(length(i), length(j))
        # Perform replacement
        for (colIdx in 1:length(j)) {
            for (rowIdx in 1:length(i)) { # two-dimensional index needs to be converted to one-dimensional index
                singleIdx <- crochet:::ijtok(x, i[rowIdx], j[colIdx])
                substr(attr(x, "_data"), singleIdx, singleIdx) <- value[rowIdx, colIdx]
            }
        }
        # Do not forget to return x
        return(x)
    }
)
```

Now we can replace some data:

```R
obj[1:7] <- "z"
obj[]
```

As you can see the simple extraction and replacement functions above cover a lot of scenarios. There are some edge cases not mentioned here that can't be handled by crochet automatically (e.g., x[FALSE], combinations with `NA`s, and so on), so if you want full coverage, you should run the crochet test suite on your custom type. Examples of this can be found in the [BEDMatrix](https://cran.r-project.org/package=BEDMatrix) or [LinkedMatrix](https://cran.r-project.org/package=LinkedMatrix) packages.


Installation
------------

Install the stable version from CRAN:

```R
install.packages("crochet")
```

Alternatively, install the development version from GitHub:

```R
# install.packages("devtools")
devtools::install_github("agrueneberg/crochet")
```


Discussion
----------

R used to export functions for index conversion such as `makeSubscript`, `vectorSubscript`, and `arraySubscript` (see [src/main/subscript.c](https://svn.r-project.org/R/trunk/src/main/subscript.c)) to package developers until R 2.3.1. These exports were removed in R 2.4.0 as part of a cleanup (https://github.com/wch/r-source/commit/7e3ce2f08807c005f930c0b36b545b10c7e9b391). `arraySubscript` was later re-added as some packages such as arules and cba still rely on it (https://github.com/wch/r-source/commit/e7f0603fe69fc972466df01d6e8d4f8c207a757b). I still need to investigate, whether `arraySubscript` would be useful for this package.


Contribute
----------

- Issue Tracker: https://github.com/agrueneberg/crochet/issues
- Source Code: https://github.com/agrueneberg/crochet


Documentation
-------------

Further documentation can be found on [RDocumentation](http://www.rdocumentation.org/packages/crochet).
