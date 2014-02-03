\name{trace.image}
\alias{trace.image}
\alias{convert.image}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Shape Tracing of an Image
}
\description{
Trace the shape of an image to create a shape signature
}
\usage{
trace.image(img, resolution)

convert.image(imgrgb, threshold = 0.5)
}
\arguments{
  \item{img}{
	a matrix of size width x height representing a black/white image with 0=white and 1=black.
}
  \item{resolution}{
The angular resolution of the trace, i.e., the length of the resulting shape signature.
}

  \item{imgrgb}{
	a matrix of size width x height x channels representing an RGB image.
}  
\item{threshold}{
	The average intensity value that serves as boundary to separate black and white pixels.
}

}
\details{
Shape signatures of objects can be created by unrolling their contour around its centroid across time. The resulting time
series represents distance-to-center of points on the contour versus
radial angle.

In order to create signatures for RGB images, convert the image with \code{convert.image} to a black-and-white image using a \code{threshold} between 0 and 1.

Exemplary datasets containing shape signatures for shape clustering are provided in this package as \code{\link{star.shapes}} and \code{\link{complex.shapes}}.
}
\value{
%\item{cb}{vector of relative frequencies}
Returns a list containing \code{angle}s and corresponding \code{distance}s from center.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Andreas M. Brandmaier
}
\seealso{
\code{\link{pdclust}}
}
\examples{
# create a filled rectangle in a 20x20 image
img <- matrix(0, nrow=20,ncol=20)
img[5:15,5:15] <- 1

# create shape signature
signature <- trace.image(img)

# plot both original image and shape signature
par(mfrow=c(1,3))
#layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
image(img)
plot(signature$angle, signature$distance,type="l",xlab="angle",ylab="distance")

# reconstruct radial plot
require("plotrix")
radial.plot(trace.image(img,resolution=500)$distance,start=0,rp.type="r",radial.lim=c(0,10))
}