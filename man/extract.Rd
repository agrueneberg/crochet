\name{extract}
\alias{extract}
\title{Create an Implementation of [ For Custom Matrix-Like Types}
\description{
\code{extract} is a function that converts different index types such as
negative integer vectors, character vectors, or logical vectors passed to the
\code{[} function as \code{i} (e.g. \code{X[i]}) or \code{i} and \code{j} (e.g.
\code{X[i, j]}) into positive integer vectors. The converted indices are
provided as the \code{i} parameter of \code{extract_vector} or \code{i} and
\code{j} parameters of \code{extract_matrix} to facilitate implementing the
extraction mechanism for custom matrix-like types.
}
\usage{
extract(extract_vector, extract_matrix, allowDoubles = FALSE)
}
\arguments{
  \item{extract_vector}{
    A function in the form of \code{function(x, i, ...)} that takes a subset of
    \code{x} based on a single index \code{i} and returns a vector.
  }
  \item{extract_matrix}{
    A function in the form of \code{function(x, i, j, ...)} that takes a subset
    of \code{x} based on two indices \code{i} and \code{j} and returns a
    matrix.
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

Optional arguments are supported and will be passed to \code{extract_vector}
and \code{extract_matrix} as long as they are named.
}
\value{
A function in the form of \code{function(x, i, j, ..., drop = TRUE)} that is
meant to be used as a method for \code{\link[base]{[}} for a custom type.
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
}
