\name{distance}
\alias{hellinger.distance}
\alias{squared.hellinger.distance}
\alias{symmetric.alpha.divergence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Codebook Dissimliarities
}
\description{
Functions to calculate distances/dissimilarities between codebooks.
}
\usage{
hellinger.distance(x,y)
squared.hellinger.distance(x,y)
symmetric.alpha.divergence(x,y)
}
\arguments{
  \item{x}{
	a codebook
}
  \item{y}{
	a codebook
}
}

\value{
Returns a numeric dissimilarity between two codebooks.
}
\details{
Note: The symmetric alpha-divergence is proportional to the Squared Hellinger distance, and is the default divergence between codebooks.
}
\author{
Andreas M. Brandmaier
}
\seealso{
\code{\link{codebook}}
}
\examples{

x <- codebook(c(sin(1:100)),m=3)
y <- codebook(c(sin(1:100*0.1)),m=3)
hellinger.distance(x,y)

}

