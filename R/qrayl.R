#*********************************************
#*********************************************
#' Returns quantile values of the Rayleigh distribution.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname qrayl
#'
qrayl<-function(p,sigma){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2008-02-11 - First version.
	# Last:  2009-05-05 - Cleaned up and implemented in the functions library due to faster execution than the corresponding functions in the package 'VGAM', and due to a problem with 'predict(arima())' when using 'VGAM'.
	########### DESCRIPTION: ###########
	# Returns quantile values of the Rayleigh distribution.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'p' contains the probability values at which the Rayleigh quantile values are to be computed.
	# - 'sigma' is the parameter in the Rayleigh distribution satisfying sigma>0.
	
	
	##################################################
	##################################################
	if(sigma<=0){
		stop("'sigma' must be positive")
		}
	sigma*sqrt(-2*log1p(-p))
	##################################################
	##################################################
	}
