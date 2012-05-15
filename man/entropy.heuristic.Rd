\name{entropy.heuristic}
\alias{entropy.heuristic}
\alias{plot.mine}
\alias{print.mine}
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
entropy.heuristic(X, m.min=3, m.max=7, t.min = 1, t.max = 1)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{X}{
A matrix representing a set of time series. Columns are time series and rows represent time points.
}
 \item{m.min}{Minimum embedding dimension}
\item{m.max}{Maximum embedding dimension}
\item{t.min}{Minimum time-delay}
\item{t.max}{Maximum time-delay}

}
\details{
For a range of embedding dimensions, the average entropy of the dataset is calculated. The embedding dimension with
the lowest entropy is chosen.  \code{print} and \code{plot} is available for result objects.

The plot of a heuristic object shows the entropy values depending on a range
of embedding dimensions and time-delays. If only embedding dimension or only
time-delay is varied, a line plot is show to indicate the parameter yielding
minimum entropy. Otherwise, an image plot is shown that indicates minimum
entropy depending on both parameters.

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

# (1)
#
# create a sine-wave with added noise
# and display a plot showing the average permutation entropy
# depending on varying choices of the embedding size
# (by default time-delay is not searched over)

heuristic <-  entropy.heuristic( sin(1:100)+rnorm(100,0,1) )
plot(heuristic)


# (2)
#
# calculate both optimal embedding dimension and time-delay
#
heuristic <-  entropy.heuristic( sin(1:100)+rnorm(100,0,1), t.min=1, t.max=6 )
plot(heuristic)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ cluster }

