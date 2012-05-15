pdc.dist <-
function(X, m=NULL, t=NULL, divergence=symmetric.alpha.divergence)
{
	if (is.null(t) | is.null(m)) {
		ent <- entropy.heuristic(X)
		
		if (is.null(m)) {
			m <- ent$m;
		}
		
		if (is.null(t)) {
			t <- ent$t;
		}
	}
	

	if (length(dim(X)) == 2) { 
		
		codebooks <- convert.matrix(X,m,t);
		D <- divergencematrix( codebooks, divergence );
	
	} else if (length(dim(X))==3) {

		codebooks <- convert.matrix.multichannel(X,m,t);
		num.channels <- dim(X)[3]
		D <- divergencematrix.multichannel( codebooks, divergence, num.channels );		
		
	} else {
		stop("Invalid dimensionality of data object!");
	}
	
	pdcdist <- as.dist(D);
	
	return(pdcdist);
}
