convert.matrix <-
function(X, m, td)
{
	#result <- c()
	#for (i in 1:dim(X)[2])
	#{
#		result <- rbind(result,codebook(X[,i],m))
#	}
#	return(result);	
	return( t(apply(X,2,codebook,m=m, t=td)) )
}
