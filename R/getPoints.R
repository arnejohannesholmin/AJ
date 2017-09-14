#' getPoints
#' @param x
#' @export
#'
getPoints=function(x){
	2*sapply(x,function(xx) sum(xx[,1]>xx[,2])) + sapply(x,function(xx) sum(xx[,1]==xx[,2]))
	}
