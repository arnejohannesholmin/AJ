#*********************************************
#*********************************************
#' Maximum likelihood estimate of a Rayleigh distributed variable.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname mlerayl
#'
mlerayl<-function(x,na.rm=TRUE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	########### DESCRIPTION: ###########
	# Maximum likelihood estimate of a Rayleigh distributed variable.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'x' is the data.
	# - 'na.rm' is TRUE if missing values are to be excluded from the estimation.
	
	
	##################################################
	##################################################
	if(na.rm){
		lx=length(x)-sum(is.na(x))
		}
	else{
		lx=length(x)
		}
	sqrt(1/2/lx*sum(x^2,na.rm=na.rm))
	##################################################
	##################################################
	}
