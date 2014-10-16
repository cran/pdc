\name{loo1nn}
\alias{loo1nn}
\title{
Leave-one-out One-nearest-neighbor Evaluation
}
\description{
Evaluates a clustering distance matrix within a supervised learning
scheme: leave-one-out one-neares-neighbor cross-validation.
This yields a rough estimate of the suitabilty of a distance function
for discriminating between classes if ground-truth is known.
}
\usage{
loo1nn(x, y)
}
\arguments{
  \item{x}{A \code{pdclust} object.}
  \item{y}{
A vector of the true class labels.
}
}
\value{
Returns a percentage-correct estimate.
}
\author{
Andreas M. Brandmaier
}


\seealso{
\code{\link{pdc.dist}}
\code{\link{pdclust}}
}
