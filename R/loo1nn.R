loo1nn <- function(x, y)
{
	stopifnot(class(x)=="pdclust" || class(x)=="dist")

	if (class(x)=="dist") {
		D <- as.matrix(x)
	} else {
		D <- as.matrix(x$D)
	}
	# set diagonal to have the largest entries
	diag(D) <- max(D)+1
	
	min.idx <- apply(D, 1, which.min)
	
	percentage <- sum(y[min.idx] == y) / length(y)
	
	return(percentage*100.0);
}