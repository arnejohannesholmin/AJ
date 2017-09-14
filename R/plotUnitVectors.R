#*********************************************
#*********************************************
#' ***DECRIPTION MISSING***
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname plotUnitVectors
#'
plotUnitVectors=function(x,y,...){
	x=rbind(0,x,NA)
	y=rbind(0,y,NA)
	plot(c(x),c(y),type="l",xlim=c(-1,1),ylim=c(-1,1),...)
	}
