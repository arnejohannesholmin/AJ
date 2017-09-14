#*********************************************
#*********************************************
#' ***DECRIPTION MISSING***
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom fields interp.surface.grid
#' @importFrom TSD NAs
#'
#' @export
#' @rdname imresize
#'
imresize<-function(im, size=0.5){
	dimim = dim(im)
	if(length(size)==1 && size>0 && size<1){
		newdim = round(dimim[1:2] * size)
		}
	else if(length(size)==1){
		size = c(size, NA)
		}
	if(length(size)>1){
		notatna = which(!is.na(size[1:2]))
		if(length(notatna)==1){
			newdim = round(dimim * size[notatna]/dimim[notatna])
			}
		else if(length(notatna)==0){
			newdim = size
			}
		}
	
	newx = seq(1, dimim[1], l=newdim[1])
	newy = seq(1, dimim[2], l=newdim[2])
	x = seq_len(dimim[1])
	y = seq_len(dimim[2])
	
	newim = NAs(newdim[1:2],dimim[3])
	for(i in seq_len(dimim[3])){
		newim[,,i] = interp.surface.grid(list(x=x, y=y, z=im[,,i]), list(x=newx, y=newy))$z
		}
	newim
	}
