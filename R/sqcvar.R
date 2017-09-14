#*********************************************
#*********************************************
#' Squared coefficient of variation.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname sqcvar
#'
sqcvar<-function(x,na.rm=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2008-05-25 - Finished.
	########### DESCRIPTION: ###########
	# Squared coefficient of variation.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'x' is an array.
	# - 'na.rm' is TRUE if NA values should be stripped before the computation proceeds.
	
	
	##################################################
	##################################################
	dimx=dim(x)
	if(length(dimx)==2){
		var(c(x),na.rm=na.rm)/mean(x,na.rm=na.rm)^2
		}
	else{
		var(x,na.rm=na.rm)/mean(x,na.rm=na.rm)^2
		}
	##################################################
	##################################################
	}
