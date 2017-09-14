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
#' @rdname cropImage
#'
cropImage=function(im,margin=c(1,1,1,1),thr=1, crop=TRUE){
	d=dim(im)
	#range1=apply(im,1,sum)
	#range2=apply(im,2,sum)
	range1 = rowSums(im[,,1]) + rowSums(im[,,2]) + rowSums(im[,,3])
	range2 = colSums(im[,,1]) + colSums(im[,,2]) + colSums(im[,,3])
	#valid1=which(range1<prod(d[2:3])*thr)
	valid1=which(range1<3*d[2]*thr)
	valid1=seq(min(valid1)-margin[1],max(valid1)+margin[2])
	valid1=valid1[valid1>0 & valid1<=d[1]]
	
	#valid2=which(range2<prod(dim(im)[c(1,3)])*thr)
	valid2=which(range2<3*d[1]*thr)
	valid2=seq(min(valid2)-margin[3],max(valid2)+margin[4])
	valid2=valid2[valid2>0 & valid2<=d[2]]
	
	if(crop){
		im[valid1,valid2,]
		}
	else{
		c(w=diff(range(valid2))+1, h=diff(range(valid1))+1, x=valid2[1], y=valid1[1])
		}
	}
