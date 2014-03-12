\name{getVector}
\alias{getVector}
\title{getVector}
\description{Get a piecewise constant vector from first up to but not including
after. This is inefficient and should be used only to compare
speed with efficient operations.}
\usage{getVector(ss, first, after)}
\arguments{
  \item{ss}{
}
  \item{first}{
}
  \item{after}{
}
}



\author{Toby Dylan Hocking}




\examples{
x <- SparseSignal(c(10, 15), c(13, 20), c(3.3, 5.5))
show <- function(first, after){
  v <- getVector(x, first, after)
  data.frame(v, i=first:(after-1))
}
show(5, 25)
show(10, 20)
show(10, 21)
show(9, 21)
}
