\name{pdclust}
\alias{pdclust}
\alias{plot.pdclust}
\alias{print.pdclust}

\title{
Permutation Distribution Clustering
}
\description{
Hierarchical cluster analysis for time series. Similarity of time series is based on the similarity of their permutation distributions. 
}
\usage{
pdclust(X, m = NULL, t = NULL, divergence =
                 symmetric.alpha.divergence, clustering.method =
                 "complete")

%Plotting function for class 'pdclust'

\method{plot}{pdclust}(x, labels=NULL, type="rectangle", cols="black",
	timeseries.as.labels = T, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
A matrix representing a set of time series. Columns represent different time series and rows represent time points.
Or: a three-dimensional array with the first dimension representing time, second dimension representing time series, and the third dimension representing channels.
}
  \item{m}{
Embedding dimension for calculating the permutation distributions. Reasonable values range somewhere between 2 and 10. If no embedding dimension is chosen, the MinE heuristic is used to determine the embedding dimension automatically.}
  \item{t}{Time-delay of the embedding.}
  \item{divergence}{
Divergence measure between discrete distributions. Default is the symmetric alpha divergence.
}
  \item{clustering.method}{
Hierarchical clustering linkage method. One out of c("complete","average","single"). 
}

For plotting:

\item{x}{A \code{pdclust} object}
\item{labels}{
Optionally provide a vector of labels for the time series here.
}
\item{type}{
One of c("triangle","rectangle") to choose the dendrogram style.
}
\item{cols}{Specify line color either as string or as vector of strings}
\item{timeseries.as.labels}{ If \code{FALSE}, a vertical dendrogram is plotted using hclust. If
\code{TRUE}, a horizontal dendrogram is plotted with time series plots as labels.}
\item{\dots}{Further graphical arguments.}
}
\details{
The function \code{pdclust} is the central function for clustering time-series in the package \code{pdc}.
It allows clustering of univariate and multivariate time-series. 
If time-series have different length, the shorter time-series can be padded
with \code{NA}s to bring them to columns of the same length in an array or a
matrix.
Multivariate time-series can also be handled by \code{pdclust}. Therefore,
the data must be transformed into a three-dimensional matrix with the
dimenions representing (1) time, (2) entities, and (3) variables/channels. 
}

\value{
Calls to \code{pdclust} return a \code{pdclust} object. There are
\code{\link{print}} and \code{\link{plot}} methods for \code{pdclust} objects.
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
\code{\link{pdc.dist}}
\code{\link{entropy.heuristic}}
\code{\link{symmetric.alpha.divergence}}
}

\examples{

# generate 5 ARIMA time series for the first group
grp1 <- replicate(5, arima.sim(n = 500, list(ar = c(0.8897, -0.4858), 
		ma = c(-0.2279, 0.2488)),
          	sd = sqrt(0.1796)) )
          
# generate 5 ARIMA time series for the second group
grp2 <- replicate(5, arima.sim(n = 500, list(ar = c(-0.71, 0.18), 
		ma = c(0.92, 0.14)),
          	sd = sqrt(0.291)) )
          
# combine groups into a single dataset
X <- cbind(grp1,grp2)

# run clustering and color original groups each in red and blue
clustering <- pdclust(X)
plot(clustering, cols=c(rep("red",5),rep("blue",5)))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ cluster }% __ONLY ONE__ keyword per line
