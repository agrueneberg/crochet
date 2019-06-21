\name{ijtok}
\alias{ijtok}
\title{Convert Two-Dimensional Indices i and j to One-Dimensional Index k}
\description{
    \code{ijtok} is a helper function that converts two-dimensional indices
    \code{i} and \code{j} to a one-dimensional index \code{k}. This can be
    useful if, for example, one-dimensional indexing is easier to implement
    than two-dimensional indexing.
}
\usage{
ijtok(x, i, j)
}
\arguments{
    \item{x}{
        A matrix-like object.
    }
    \item{i}{
        The first component of a two-dimensional index.
    }
    \item{j}{
        The second component of a two-dimensional index.
    }
}
\details{
    It is assumed that all indices are one-based.
}
\value{
    A one-dimensional index.
}
