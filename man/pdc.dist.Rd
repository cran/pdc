\name{pdc.dist}
\alias{pdc.dist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Permutation Distribution Clustering Distance Matrix
}
\description{
This function computes and returns the distance matrix computed by the divergence between permutation distributions of time series.
}
\usage{
pdc.dist(X, m = NULL, t = NULL, divergence = symmetric.alpha.divergence)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
A matrix representing a set of time series. Columns are time series and rows represent time points.

}
  \item{m}{
Embedding dimension for calculating the permutation distributions. 
Reasonable values range usually somewhere between 2 and 10. If no embedding dimension is chosen, the MinE heuristic is used to determine the embedding dimension automatically.

}
\item{t}{Time-delay of the embedding}
  \item{divergence}{
Divergence measure between discrete distributions. Default is the symmetric alpha divergence.

}
}
\details{
A valid divergence is always non-negative.
}
\value{
Returns the dissimilarity between two codebooks as floating point number (larger or equal than zero).
}
\references{
%% ~put references to the literature/web site here ~
Brandmaier, A. M. \emph{Permutation Distribution Clustering and Structural Equation Model Trees}. Dissertation. Saarland University. Saarbruecken. 2012.

}
\author{
Andreas M. Brandmaier
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{pdclust}}

\code{\link{hclust}}
\code{\link{kmeans}}

}
\examples{

# create a set of time series consisting
# of sine waves with different degrees of added noise
# and two white noise time series
X <- cbind(
sin(1:500)+rnorm(500,0,.1),
sin(1:500)+rnorm(500,0,.2),
sin(1:500)+rnorm(500,0,.3),
sin(1:500)+rnorm(500,0,.4),
rnorm(500,0,1),
rnorm(500,0,1)
)

# calculate the distance matrix
D <- pdc.dist(X,3)

# and plot with lattice package, you will
# be able to spot two clusters: a noise cluster
# and a sine wave cluster
require("lattice")
levelplot(as.matrix(D), col.regions=grey.colors(100,start=0.9, end=0.3))


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ cluster }% __ONLY ONE__ keyword per line
