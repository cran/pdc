pdc.dist <-
function(X, m=NULL, divergence=symmetric.alpha.divergence)
{
	if (is.null(m)) {
		m <- entropy.heuristic(X)$m;
	}
	
	codebooks <- convert.matrix(X,m);
	D <- divergencematrix( codebooks, divergence );
	
	pdcdist <- as.dist(D);
	
	return(pdcdist);
}
