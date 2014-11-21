\name{rasterPlot}
\alias{rasterPlot}

\title{Dendrogram Plot with Images} 

\description{Plots a horizontal dendrogram with images as leafs}

\usage{
	rasterPlot(cl, raw, monochrome=FALSE, aspect, \dots) 
}

\arguments{
	\item{cl}{A \code{\link{pdclust}} object.} 
	\item{raw}{List of RGB images in matrix format.}
	\item{monochrome}{Convert image to b/w representation.}
	\item{aspect}{Aspect ratio.}
	\item{\dots}{Further graphical arguments.}
}

\author{
Andreas M. Brandmaier
}
\references{
Brandmaier, A. M. (in press). pdc: An R Package for Complexity-Based Clustering
of Time Series. Journal of Statistical Software.
}

\seealso{
\code{\link{pdclust}} 
}

\examples{
data("complex.shapes")
data("complex.shapes.raw")
clust <- pdclust(complex.shapes, t=5)
rasterPlot(clust, complex.shapes.raw$images)
}