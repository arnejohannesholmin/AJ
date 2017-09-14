#*********************************************
#*********************************************
#' Returns probability density values of the Rayleigh distribution.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname drayl
#'
drayl<-function(x,sigma){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2008-02-11 - First version.
	# Last:  2009-05-05 - Cleaned up and implemented in the functions library due to faster execution than the corresponding functions in the package 'VGAM', and due to a problem with 'predict(arima())' when using 'VGAM'.
	########### DESCRIPTION: ###########
	# Returns probability density values of the Rayleigh distribution.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'x' contains the values at which the Rayleigh density values are to be computed.
	# - 'sigma' is the parameter in the Rayleigh distribution satisfying sigma>0.
	
	
	##################################################
	##################################################
	# 'x' needs to be non-negative:
	x=replace(x,x<0,0)
	# 'sigma' needs to be positive:
	if(sigma<=0){
		stop("'sigma' must be positive")
		}
	x*exp(-x^2/2/sigma^2)/sigma^2
	##################################################
	##################################################
	}
