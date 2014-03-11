\name{SparseSignal}
\alias{SparseSignal}
\title{SparseSignal}
\description{Make a SparseSignal, which is a piecewise constant function
defined on 1, ..., L. }
\usage{SparseSignal(first, after, value)}
\arguments{
  \item{first}{Integer vector: first position of each segment.}
  \item{after}{Integer vector: first position after each segment.}
  \item{value}{Numeric vector: segment values.}
}

\value{data.frame with columns first, after, value. 1 row per segment,
and there are no rows for segments with value 0.}

\author{Toby Dylan Hocking}




\examples{
## SparseSignals do not have 0-valued segments.
ss <- SparseSignal(c(10, 12, 15, 1000),
                   c(11, 13, 17, 1001),
                   c( 1, -2,  4,    0))
print(ss)
}
