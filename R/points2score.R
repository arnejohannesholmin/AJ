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
#' @rdname points2score
#'
points2score=function(x,pow=pow,w=20,maxp=900,bronze=TRUE){
	out=round((x/maxp)^pow * w)
	if(!bronze && length(out)>3){
		out[3:4]=floor(mean(out[3:4]))
		}
	out
	}
