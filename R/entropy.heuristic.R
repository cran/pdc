entropy.heuristic <-
function(X, m.min=3, m.max=8)
{
	X <- as.matrix(X)
	

	ent <- rep(0, m.max-m.min+1)
	for (i in m.min:m.max){
		#ent[i-m.min+1] <- mean(sapply(FUN=codebook.entropy, datalist,m=i))
		ent[i-m.min+1] <- mean(apply(FUN=codebook.entropy, MARGIN=2, X, m=i))
	}
	best <- which.min(ent);
	
	result <- list()
	result$entropy.values <- ent
	result$m <- best+m.min-1
	result$entropy.ms <- m.min:m.max
	
	return (result);
}
