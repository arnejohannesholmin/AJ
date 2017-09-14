#*********************************************
#*********************************************
#' Coefficient of variation.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom stats sd
#'
#' @export
#' @rdname cvar
#'
cvar<-function(x,na.rm=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2008-05-25 - Finished.
	########### DESCRIPTION: ###########
	# Coefficient of variation.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'x' is an array.
	# - 'na.rm' is TRUE if NA values should be stripped before the computation proceeds.
	
	
	##################################################
	##################################################
	dimx=dim(x)
	if(length(dimx)==2){
		sd(c(x),na.rm=na.rm)/mean(x,na.rm=na.rm)
		}
	else{
		sd(x,na.rm=na.rm)/mean(x,na.rm=na.rm)
		}
	##################################################
	##################################################
	}
