pdclust <-
function(X, m=NULL, divergence=symmetric.alpha.divergence, clustering.method="complete") 
{
	user.m <- !is.null(m);
	if (is.null(m)) {
		m <- entropy.heuristic(X)$m;
	}

	# calculate divergence matrix			
	D <- pdc.dist(X,m,divergence);
	
	# start hierarchical clustering
	if (clustering.method == "complete") {
		hcl <- hclust(D, method="complete")
	} else if (clustering.method == "average") {
		hcl <- hclust(D, method="average")
	}  else if (clustering.method == "single") {
		hcl <- hclust(D, method="single")
	} else {
		stop("Invalid clustering method!")
	}
	
	# add meta info
	hcl$divergence <- divergence
	hcl$m <- m
	hcl$user.specified.m <- user.m
	hcl$N <- length(hcl$order)
	hcl$data <- X
	
	# wrap hclust result
	class(hcl) <- "pdclust"
	
	return(hcl);	
}
