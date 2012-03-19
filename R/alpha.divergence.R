alpha.divergence <-
function(dist1, dist2)
{
	return ( 1-sum(sqrt(dist1*dist2)) )
	
}
