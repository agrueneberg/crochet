\docType{package}
\name{crochet-package}
\alias{crochet-package}
\title{
\packageTitle{crochet}
}
\description{
\packageDescription{crochet}
}
\section{Discussion}{
R used to export functions for index conversion such as \code{makeSubscript},
\code{vectorSubscript}, and \code{arraySubscript} in
\code{src/main/subscript.c} to package developers until R 2.3.1. These exports
were removed in R 2.4.0 as part of a
\href{https://github.com/wch/r-source/commit/7e3ce2f08807c005f930c0b36b545b10c7e9b391}{cleanup}.
\code{arraySubscript} was later
\href{https://github.com/wch/r-source/commit/e7f0603fe69fc972466df01d6e8d4f8c207a757b}{re-added}
as some packages such as arules and cba still rely on it. I still need to
investigate whether \code{arraySubscript} would be useful for this package.
}
\seealso{
\code{\link{extract}} and \code{\link{replace}} to create an implementation of
\code{[} and \code{[<-} for custom types, respecively.
\code{vignette("StringMatrix", package = "crochet")} for a vignette containing
a full example for a custom type.
}
\keyword{package}
