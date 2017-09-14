#*********************************************
#*********************************************
#' Applies a Gaussian 3-D kernel on MS70 sonar data given as the coordinates 'x' and the data 'y', and the bandwidths given in the three element vector 'h':
#'
#' @param x  is the matrix of the coordinates.
#' @param y  is the vector of the data, with dimension corresponding to the number of columns if 'x'.
#' @param h  is the three element vector of bandwidths.
#' @param w  is the limits of the kernel, outside which the kernel is 0.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD dim_all ones
#'
#' @export
#' @rdname kernSmoothGauss
#'
kernSmoothGauss=function(x,y,h=1,w=h*3){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2012-01-24 - Clean version.
	########### DESCRIPTION: ###########
	# Applies a Gaussian 3-D kernel on MS70 sonar data given as the coordinates 'x' and the data 'y', and the bandwidths given in the three element vector 'h':
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is the matrix of the coordinates.
	# ---y--- is the vector of the data, with dimension corresponding to the number of columns if 'x'.
	# ---h--- is the three element vector of bandwidths.
	# ---w--- is the limits of the kernel, outside which the kernel is 0.
		

	##################################################
	##################################################
	##### Preparation, execution #####
	if(h==0){
		return(list(x=x,y=y))
		}
	if(any(is.na(x)) || any(is.na(y))){
		stop("NAs not allowed")
		}
	D=NCOL(x)
	d=dim_all(y)
	nd=length(d)
	if(nd!=D){
		stop("'y' needs to be an array of ",D," dimensions")
		}
	olddim=d
	
	
	if(nd<3){
		d=c(d,ones(3-nd))
		x=cbind(x,ones(length(y),3-nd))
		}
	h=rep(h,length.out=3)
	w=rep(w,length.out=3)
	
	
	# Execution:
	U <- .C("kernSmooth3dGaussMultipleNoNA", as.double(x[,1]), as.double(x[,2]), as.double(x[,3]), as.double(y), as.integer(d[1]), as.integer(d[2]), as.integer(d[3]), as.integer(1), as.double(h[1]), as.double(h[2]), as.double(h[3]), as.double(w[1]), as.double(w[2]), as.double(w[3]), as.double(y), PACKAGE="sonR")
	y = U[[15]]
	# Insert NAs:
	
	# Reset dimension to the original:
	dim(y)=olddim
	
		
	##### Output #####
	list(x=x,y=y)
	##################################################
	##################################################
	}
