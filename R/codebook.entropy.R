codebook.entropy <-
function(data, m)
{

	ent <- entropy(codebook(data,m))
	
	return( ent);
}
