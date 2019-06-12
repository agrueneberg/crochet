\name{convertIndex}
\alias{convertIndex}
\title{Convert Non-Numeric Index Types to Positive Integers}
\description{
Converts different index types such as negative integer vectors, character
vectors, or logical vectors into positive integer vectors.
}
\usage{
convertIndex(x, i, type, allowDoubles = FALSE)
}
\arguments{
  \item{x}{
    A matrix-like object.
  }
  \item{i}{
    The index to convert: may be a one-dimensional or two-dimensional logical,
    character, integer, or double vector.
  }
  \item{type}{
    The type of index to convert to: \code{k} is a one-dimensional index,
    \code{i} is the part of a two-dimensional index that determines the rows,
    and \code{j} is the part of a two-dimensional index that determines the
    columns.
  }
  \item{allowDoubles}{
    If set, indices of type double are not converted to integers if the
    operation would overflow to support matrices with \code{nrow()},
    \code{ncol()}, or \code{length()} greater than the largest integer that can
    be represented (\code{.Machine$integer.max}).
  }
}
\value{
The converted index.
}
\seealso{
\code{\link{extract}} and \code{\link{replace}} to generate implementations for
\code{[} and \code{[<-} for custom types that use \code{convertIndex} under the
hood.
}
\examples{
x <- matrix(data = rnorm(25), nrow = 5, ncol = 5)
dimnames(x) <- list(letters[1:5], letters[1:5])

convertIndex(x, c(1, 2, 3), "k")
convertIndex(x, -25, "k")
convertIndex(x, c(TRUE, FALSE), "k")

convertIndex(x, c(1, 2, 3), "i")
convertIndex(x, -5, "i")
convertIndex(x, c(TRUE, FALSE), "i")
convertIndex(x, c("a", "b", "c"), "i")

convertIndex(x, c(1, 2, 3), "j")
convertIndex(x, -5, "j")
convertIndex(x, c(TRUE, FALSE), "j")
convertIndex(x, c("a", "b", "c"), "j")
}
