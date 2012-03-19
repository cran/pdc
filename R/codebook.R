codebook <-
function(x,m=3)
{
	#cb <- rep(-1,length(v)-m+1)
	distribution <- rep(0, factorial(m))
	for(i in 1:(length(x)-m+1))
	{
		#cat(i)
		result <- sort(x[i:(i+m-1)],index.return=T)
		permutation <- result$ix-1
		sorteddata <- result$x
		sortedlist <- (1:m)-1
		#cat(perm,"\n")
		
		number <- 1
		for (j in 0:(m-1))
		{
			#cat("SL",sortedlist,"\n",sorteddata,"\nperm",permutation,"\n")
			idx <- which(sortedlist==permutation[j+1])[1]
			sortedlist <- sortedlist[-idx]
			number <- number + factorial(m-j-1)*(idx-1)
			
		#	cat("IDX",idx,"\n\n")
			
		}	
		#cat(number,"\n");
		#cb[i] <- number
		distribution[number] <- distribution[number] + 1
	}
	
	return(distribution/sum(distribution))
	
}
