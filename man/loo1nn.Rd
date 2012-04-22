\name{loo1nn}
\alias{loo1nn}
%- Also NEED an '\alias' for EACH other topic documented here.
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
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A \code{pdclust} object.}
  \item{y}{
A vector of the true class labels.
}
}
%\details{
%}
\value{
Returns a percentage-correct estimate.
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
\code{\link{pdc.dist}}
\code{\link{pdclust}}
}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
