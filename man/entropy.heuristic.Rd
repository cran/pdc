\name{entropy.heuristic}
\alias{entropy.heuristic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Minimum Entropy Heuristic (MinE)
}
\description{
The information content of a permutation distribution depends crucially
on the choice of the embedding dimension. Too small embedding dimensions
narrow the representational power of the distribution, too large embedding
dimensions dilute the estimation of the distribution. The Minimum Entropy
Heuristic (MinE) automatically chooses an embedding dimension with an optimal
representational entropy as proxy for representational power.
}
\usage{
entropy.heuristic(X, m.min=3, m.max=8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
A matrix representing a set of time series. Columns are time series and rows represent time points.
}
 \item{m.min}{Minimum embedding dimension}
\item{m.max}{Maximum embedding dimension}

}
\details{
For a range of embedding dimensions, the average entropy of the dataset is calculated. The embedding dimension with
the lowest entropy is chosen.  
}
\value{
A list is returned with the following elements:
\item{m}{The chosen embedding size.}
\item{entropy.values}{A vector with average entropy values corresponding to each entry in \code{entropy.ms}}
\item{entropy.ms}{A vector of the embedding dimensions that were searched for the optimal embedding.}
}
\references{
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
}
\examples{

# create a sine-wave with added noise
# and display a plot showing the average permutation entropy
# depending on varying choices of the embedding size

heur <-  entropy.heuristic( sin(1:100)+rnorm(100,0,1) )
pch <- rep(22, length(heur$entropy.ms))
pch[ which(heur$entropy.ms==heur$m)] <- 15
plot(heur$entropy.ms, heur$entropy.values, type="b", pch=pch,
	xlab="embedding dimension", ylab="entropy")
abline(v = heur$m, lty=3)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ cluster }

