\name{addSparseSignals}
\alias{addSparseSignals}
\title{addSparseSignals}
\description{Add two SparseSignals.}
\usage{addSparseSignals(x, y)}
\arguments{
  \item{x}{
}
  \item{y}{
}
}



\author{Toby Dylan Hocking}




\examples{
x <- data.frame(1L, 2L, 5.5)
addSparseSignals(x, data.frame(4L, 10L, 3.8))

neg <- data.frame(1L, 2L, -5.5)
addSparseSignals(neg, x)

x <- data.frame(c(10L, 12L, 14L),
                c(11L, 13L, 16L),
                c(  1,   2,   3))
y <- data.frame(c(10L, 12L, 15L),
                c(11L, 13L, 17L),
                c( 1, -2,    4))
addSparseSignals(x, y)
}
