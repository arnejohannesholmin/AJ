#' spacepadfirst
#' @param x
#' @export
#'
spacepadfirst=function(x,l=NULL){
	ncharx=nchar(x)
	if(length(l)==0){
		l=ncharx
		}
	else if(length(l)!=length(x)){
		l=rep(l,length.out=length(x))
		}
	if(any(ncharx>l)){
		warning("The length 'l' must exceed tne number of characters. 'l' set to nchar(x)")
		l=ncharx
		}
	out=x
	for(i in seq_along(x)){
		x[i] = paste(paste(rep(" ",l=l[i]-ncharx[i]),collapse=""), x[i], sep="")
		}
	x
	}
