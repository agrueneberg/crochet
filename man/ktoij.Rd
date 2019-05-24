\name{ktoij}
\alias{ktoij}
\title{Convert One-Dimensional Index k to Two-Dimensional Indices i and j}
\description{
\code{ktoij} is a helper function that converts a one-dimensional index
\code{k} to two-dimensional indices \code{i} and \code{j}. This can be useful
if, for example, two-dimensional indexing is easier to implement than
one-dimensional indexing.
}
\usage{
ktoij(x, k)
}
\arguments{
  \item{x}{
    A matrix-like object.
  }
  \item{k}{
    A one-dimensional index.
  }
}
\details{
It is assumed that all indices are one-based.
}
\value{
A list containing indices \code{i} and \code{j}.
}
