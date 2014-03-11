\name{-.SparseSignal}
\alias{-.SparseSignal}
\title{- SparseSignal}
\description{Subtract two SparseSignals.}
\usage{\method{-}{SparseSignal}(x, y)}
\arguments{
  \item{x}{
}
  \item{y}{
}
}



\author{Toby Dylan Hocking}




\examples{
## subtracting two non-overlapping 1-segment signals gives a signal
## with 2 segments.
x <- SparseSignal(1, 2, 5.5)
x - SparseSignal(4, 10, 3.8) # 2 rows.
## subtracting a SparseSignal from itself gives an empty signal.
none <- x - x
str(none)
}
