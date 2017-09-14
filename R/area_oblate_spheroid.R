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
#' @rdname area_oblate_spheroid
#'
area_oblate_spheroid<-function(axes,exact=TRUE,p=1.6075){
	if(!identical(ncol(axes),3)){
		axes=matrix(axes,length(axes)/3,3,byrow=TRUE)
		}
	# Exact calculation:
	if(exact){
		alpha=acos(axes[,3]/axes[,1])
		2*pi* (axes[,1]^2 +axes[,3]^2/sin(alpha) * log((1+sin(alpha)) / cos(alpha)))
		}
	# Approximate calculation by the Knud Thomsen's formula:
	else{
		4*pi * ((axes[,1]^p*axes[,2]^p + axes[,2]^p*axes[,3]^p + axes[,1]^p*axes[,3]^p)/3)^(1/p)
		}
	}
