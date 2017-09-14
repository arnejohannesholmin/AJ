#*********************************************
#*********************************************
#' Orders the columns given of the matrix 'x' by the columns given by 'by'. This function is the same as order(col1,col2,col3,...) only with a more robust argument list, where the matrix 'x' and the order 'by' in which columns are ordered are given.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD zeros
#'
#' @export
#' @rdname sortby
#'
sortby<-function(x,by=1,return.order=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-07-31 - Clean version. (see Notes nr 44)
	########### DESCRIPTION: ###########
	# Orders the columns given of the matrix 'x' by the columns given by 'by'. This function is the same as order(col1,col2,col3,...) only with a more robust argument list, where the matrix 'x' and the order 'by' in which columns are ordered are given.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'x' is the input matrix to be sorted.
	# - 'by' a vector of indices of the columns to be sorted. The fisrt column given by 'by' is sorted most slowly.  
	# - 'return.order' is TRUE if a list of the ordered matrix 'x' and the order is to be returned.
	
	
	##################################################
	##################################################
	##### Preparation #####
	# Variables limiting the memory usage of the function:
	maxcol=20
	limit=1e7
	# Dimensions of the input:
	ncolx=ncol(x)
	nrowx=nrow(x)
	
	# Only matrices are treated:
	if(length(dim(x))!=2){
		stop("Only matrices can be sorted")
		}
	# The function only takes matrices of fewer than 21 columns. If larger matrices must be ordered, alter the function by adding conditional expressions for n==25, n==30 and so forth:
	if(ncolx>maxcol){
		stop("Too many columns. The function need to be altered.")
		}
	# 'n' is the number of arguments (in grid of 5) used in order in the conditional expressions below:
	n=ceiling(ncolx/5)*5
	# Total number of bytes occupied by the function:
	N=nrowx*n
	if(N>limit){
		continue=readline(paste("The function will allocate a vector of size ",N*8," bytes in double mode. Continue? (y/n)",sep=""))
		if(tolower(continue)=="n"){
			return(NULL)
			}
		}
	
	# If 'by' is NULL, the standard ordering is chosen:
	if(!all(by%in%1:ncolx)){
		stop("'by' contains indexes out of bounds of 'x'")
		}
	if(is.null(by)){
		by=1:ncolx
		}
	if(length(by)<ncolx){
		by=c(by,(1:ncolx)[-by])
		}
	
	
	##### Execution #####	
	# The ordering:
	if(n==5){
		colmissimg=n-ncolx
		zeros=double(nrowx*colmissimg)
		dim(zeros)=c(nrowx,colmissimg)
		x=cbind(x,zeros)
		ox=order(x[,by[1]],x[,by[2]],x[,by[3]],x[,by[4]],x[,by[5]])
		}
	if(n==10){
		colmissimg=n-ncolx
		zeros=double(nrowx*colmissimg)
		dim(zeros)=c(nrowx,colmissimg)
		x=cbind(x,zeros)
		ox=order(x[,by[1]],x[,by[2]],x[,by[3]],x[,by[4]],x[,by[5]],x[,by[6]],x[,by[7]],x[,by[8]],x[,by[9]],x[,by[10]])
		}
	if(n==15){
		colmissimg=n-ncolx
		zeros=double(nrowx*colmissimg)
		dim(zeros)=c(nrowx,colmissimg)
		x=cbind(x,zeros)
		ox=order(x[,by[1]],x[,by[2]],x[,by[3]],x[,by[4]],x[,by[5]],x[,by[6]],x[,by[7]],x[,by[8]],x[,by[9]],x[,by[10]],x[,by[11]],x[,by[12]],x[,by[13]],x[,by[14]],x[,by[15]])
		}
	if(n==20){
		colmissimg=n-ncolx
		zeros=double(nrowx*colmissimg)
		dim(zeros)=c(nrowx,colmissimg)
		x=cbind(x,zeros)
		ox=order(x[,by[1]],x[,by[2]],x[,by[3]],x[,by[4]],x[,by[5]],x[,by[6]],x[,by[7]],x[,by[8]],x[,by[9]],x[,by[10]],x[,by[11]],x[,by[12]],x[,by[13]],x[,by[14]],x[,by[15]],x[,by[16]],x[,by[17]],x[,by[18]],x[,by[19]],x[,by[20]])
		}
	
	
	##### Output #####
	if(return.order){
		list(x=x[ox,1:ncolx,drop=FALSE],order=ox)
		}
	else{
		x[ox,1:ncolx,drop=FALSE]
		}
	##################################################
	##################################################
	}
