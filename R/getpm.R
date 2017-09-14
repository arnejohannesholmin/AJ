#' getpm
#' @param x
#' @export
#'
getpm=function(x){
	sapply(x,function(xx) sum(xx[,1])) - sapply(x,function(xx) sum(xx[,2]))
	}
