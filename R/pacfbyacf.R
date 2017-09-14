#*********************************************
#*********************************************
#' PACF based on ACF.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname pacfbyacf
#'
pacfbyacf<-function(acf,...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 20009-02-28 - Draft on excercise STAT211.
	# Last:  2009-05-12 - Finished.
	########### DESCRIPTION: ###########
	# PACF based on ACF.
	############ VARIABLES: ############
	# - 'acf' is the input autocovariance or autocorrelation vector of the dezired length + 1.
	# - '...' are graphical parameters of the plot function.
	
	
	##################################################
	##################################################
	if(identical(class(acf),"acf")){
		acf=as.vector(acf$acf)
		}
	lacf=length(acf)
	a=double(lacf)
	for(h in 1:lacf){
		mal=abs(matrix(0:(h-1),h,h)-matrix(0:(h-1),h,h,byrow=TRUE))+1
		acfM=matrix(acf[mal],h,h)
		acfh=acf[2:(h+1)]
		phi=solve(acfM)%*%acfh
		a[h]=phi[h]
		}
	plot(a,type="h",ylab="PACF",xlab="Lag",...)
	abline(h=0)
	##################################################
	##################################################
	}
