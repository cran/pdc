\name{codebook}
\alias{codebook}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Codebook
}
\description{
A codebook contains the permutation distribution of a time series.
}
\usage{
codebook(x, m = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	a vector ot a time series
}
  \item{m}{
The embedding dimension.
}
}
\details{
The length of a codebook is the factorial of the embedding dimension. The elements of the
codebook represent relative frequencies of codewords of size m.
}
\value{
%\item{cb}{vector of relative frequencies}
Returns a vector of relative frequencies.
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
\code{\link{pdclust}}
}
\examples{

# calculate codebook from sine-wave
cb <- codebook(c(sin(1:100)),m=3)

# plot the permutation distribution
barplot(cb,xlab="Permutation Distribution")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ cluster }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
