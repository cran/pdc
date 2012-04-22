print.mine <- function(x, ...)
{
	cat(paste("Embedding dimension: ",x$m, "[",paste(x$m.range,collapse=","),"]   \nTime delay: ",
	x$t, "[",paste(x$t.range,collapse=","),"]"));
}