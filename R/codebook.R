codebook <-
function(x,m=3,t=1)
{

	if (t*(m-1) >= length(x)) {
		return(NA);
	}

	# generic codeword function
	codeword.func <- codeword;
	# specialied codeword functions (for speed up)
	if (m==2) {
		codeword.func <- codeword2;
	} else if (m==3) {
		codeword.func <- codeword3;
	} else if (m==4) {
		codeword.func <- codeword4;
	} else if (m==5) {
		codeword.func <- codeword5;
	} else if (m==6) {
		codeword.func <- codeword6;
	} else if (m==7) {
		codeword.func <- codeword7;
	}
	#codeword.func <- codeword;

	distribution <- rep.int(0, factorial(m))
	to <- (length(x)-t*(m-1))
	for(i in 1:to)
	{

		data <- x[seq.int(i,i+t*(m-1),t)]
		
		# skip data containing NA
		if	(any(is.na(data))) { next; }
		
		# calculate permutation index
		number <- codeword.func(data, m);
				
		distribution[number] <- distribution[number] + 1
	}
	
	return(distribution/sum(distribution))
	
}
