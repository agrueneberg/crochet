\name{replace}
\alias{replace}
\title{Create an Implementation of [<- For Custom Matrix-Like Types}
\description{
\code{replace} is a function that converts different index types such as
negative integer vectors, character vectors, or logical vectors passed to the
\code{[<-} function as \code{i} (e.g. \code{X[i]}) or \code{i} and \code{j}
(e.g. \code{X[i, j]}) into positive integer vectors. The converted indices are
provided as the \code{i} parameter of \code{replace_vector} or \code{i} and
\code{j} parameters of \code{replace_matrix} to facilitate implementing the
replacement mechanism for custom matrix-like types. Values are recycled to
match the replacement length.
}
\usage{
replace(replace_vector, replace_matrix, allowDoubles = FALSE)
}
\arguments{
  \item{replace_vector}{
    A function in the form of \code{function(x, i, ..., value)} that replaces a
    vector subset of \code{x} based on a single index \code{i} with the values
    in \code{value} and returns \code{x}.
  }
  \item{replace_matrix}{
    A function in the form of \code{function(x, i, j, ..., value)} that
    replaces a matrix subset of \code{x} based on two indices \code{i} and
    \code{j} with the values in \code{value} and returns \code{x}.
  }
  \item{allowDoubles}{
    If set, indices of type double are not converted to integers if the
    operation would overflow to support matrices with \code{nrow()},
    \code{ncol()}, or \code{length()} greater than the largest integer that can
    be represented (\code{.Machine$integer.max}).
  }
}
\details{
The custom type must implement methods for \code{\link[base]{length}},
\code{\link[base]{dim}} and \code{\link[base]{dimnames}} for this function to
work.  Implementing methods for \code{\link[base]{nrow}},
\code{\link[base]{ncol}}, \code{\link[base]{rownames}}, and
\code{\link[base]{colnames}} is not necessary as the default method of those
generics calls \code{\link[base]{dim}} or \code{\link[base]{dimnames}}
internally.
}
\value{
A function in the form of \code{function(x, i, j, ..., value)} that is meant to
be used as a method for \code{\link[base]{[<-}} for a custom type.
}
\seealso{
\code{vignette("StringMatrix", package = "crochet")} for a vignette containing
a complete example on how to use \code{replace} to implement \code{[<-} for a
custom type.
}
\examples{
b <- matrix(data = rnorm(25), nrow = 5, ncol = 5)
dimnames(b) <- list(letters[1:5], letters[1:5])

a <- structure(list(), class = "TestMatrix")

dim.TestMatrix <- function(x) {
    dim(b)
}

dimnames.TestMatrix <- function(x) {
    dimnames(b)
}

extract_vector <- function(x, i) {
    # Dispatch to b instead to x for this demo
    b[i, drop = FALSE]
}

extract_matrix <- function(x, i, j) {
    # Dispatch to b instead to x for this demo
    b[i, j, drop = FALSE]
}

`[.TestMatrix` <- extract(extract_vector = extract_vector, extract_matrix = extract_matrix)

replace_vector <- function(x, i, value) {
    .GlobalEnv$i <- i
    .GlobalEnv$value <- value
    # Dispatch to b instead to x for this demo
    with(.GlobalEnv, b[i] <- value)
    # Don't forget to return x
    return(x)
}

replace_matrix <- function(x, i, j, value) {
    .GlobalEnv$i <- i
    .GlobalEnv$j <- j
    .GlobalEnv$value <- value
    # Dispatch to b instead to x for this demo
    with(.GlobalEnv, b[i, j] <- value)
    # Don't forget to return x
    return(x)
}

`[<-.TestMatrix` <- replace(replace_vector = replace_vector, replace_matrix = replace_matrix)
}
