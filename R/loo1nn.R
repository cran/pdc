loo1nn <- function(x, y)
{
	stopifnot(class(x)=="pdclust")

	D <- as.matrix(x$D)
	# set diagonal to have the largest entries
	diag(D) <- max(D)+1
	
	min.idx <- apply(D, 1, which.min)
	
	percentage <- sum(y[min.idx] == y) / length(y)
	
	return(percentage*100.0);
}