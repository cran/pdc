pdclust <-
function(X, m=NULL, t=NULL, divergence=symmetric.alpha.divergence, clustering.method="complete") 
{
	user.m <- !is.null(m);
	user.t <- !is.null(t);
	
	if ((is.null(m)) && (is.null(t))) {
			m <- entropy.heuristic(X)$m;
	}
	
	if (is.null(m)) {
			m <- entropy.heuristic(X, t.min=t, t.max=t)$m;
	}
	
	if (is.null(t)) {
			t <- 1;
	}
	


	# calculate divergence matrix			
	D <- pdc.dist(X,m,t,divergence);
	
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
	hcl$user.specified.t <- user.t
	hcl$N <- length(hcl$order)
	hcl$data <- X
	hcl$D <- D
	
	# wrap hclust result
	class(hcl) <- "pdclust"
	
	return(hcl);	
}
