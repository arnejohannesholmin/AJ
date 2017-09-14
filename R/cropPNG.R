#*********************************************
#*********************************************
#' ***DECRIPTION MISSING***
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD dim_all
#' @importFrom tools file_ext
#'
#' @export
#' @rdname cropPNG
#'
cropPNG<-function(x, xout=NULL, margin=rep(4,4), thr=1, ...){
	require(png)
	if(isTRUE(file.info(x)$isdir)){
		x=list.files(x,full.names=TRUE)
		}
	
	x=x[tolower(file_ext(x))=="png"]
	if(length(xout)==0){
		xout=gsub(".png","_cropped.png",x,fixed=TRUE)
		}
	else if(length(xout)==1 && tolower(file_ext(xout))!="png"){
		if(!file.exists(xout)){
			dir.create(xout)
			}
		xout = file.path(xout, basename(x))
		}
	for(i in seq_along(x)){
		im=readPNG(x[i])
		printt(dim_all(im))
		im_cropped=cropImage(im,margin=margin,thr=thr)
		printt(dim_all(im_cropped))
		# Save as png:
		writePNG(im_cropped,xout[i],...)
		}
	}
