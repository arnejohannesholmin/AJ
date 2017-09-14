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
#' @rdname arrowsl
#'
arrowsl <- function(x0, y0, x1 = x0, y1 = y0, l=1, length = 0.25, angle = 30, code = 2, col = par("fg"), lty = par("lty"), lwd = par("lwd"), ...){
	lens = sqrt((x1-x0)^2 + (y1-y0)^2)
	angs = atan((y1-y0)/(x1-x0))
	
	# Crop the lengths of the arrows to 'l':
	if(length(l)==1){
		l = rep(lens * (1-l)/2, 2)
		}
	l = cbind(l[1]*cos(ang) * sign(x1-x0), l[2]*sin(ang) * sign(y1-y0))
	
	x0 = x0 + l[,1]
	x1 = x1 - l[,1]
	y0 = y0 + l[,2]
	y1 = y1 - l[,2]
	
	arrows(x0, y0, x1=x1, y1=y1, length=length, angle=angle, code=code, col=col, lty=lty, lwd=lwd, ...)
	}
