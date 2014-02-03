hellinger.distance <-
function(x, y)
{
	return( 1/(sqrt(2))* sqrt( sum( (sqrt(x)-sqrt(y))**2 ) ))
	
}
