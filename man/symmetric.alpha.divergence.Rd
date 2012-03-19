\name{symmetric.alpha.divergence}
\alias{symmetric.alpha.divergence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Symmetric alpha divergence
}
\description{
Symmetric alpha divergence between discrete distributions
}
\usage{
symmetric.alpha.divergence(dist1, dist2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dist1}{
	codebook of a time series
}
  \item{dist2}{
	codebook of a time series
}
}
\details{
The symmetric alpha-divergence is proportional to the Squared Hellinger distance.
}
\value{
\item{d}{Divergence between two permutation distributions}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Andreas M. Brandmaier
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{pdclust}}
}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ divergence }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
