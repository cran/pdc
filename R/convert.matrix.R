convert.matrix <-
function(X, m)
{
	result <- c()
	for (i in 1:dim(X)[2])
	{
		result <- rbind(result,codebook(X[,i],m))
	}
	return(result);	
}
