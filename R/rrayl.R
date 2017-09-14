#*********************************************
#*********************************************
#' Draws from the Rayleigh distribution.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom stats runif
#'
#' @export
#' @rdname rrayl
#'
rrayl<-function(n,sigma){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2008-02-11 - First version.
	# Last:  2009-05-05 - Cleaned up and implemented in the functions library due to a problem with 'predict(arima())' when using 'VGAM'.
	########### DESCRIPTION: ###########
	# Draws from the Rayleigh distribution.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'n' is the number of observations to generate from the Rayleigh distribution.
	# - 'sigma' is the parameter in the Rayleigh distribution satisfying sigma>0.
	
	
	##################################################
	##################################################
	if(any(sigma<=0)){
		stop("'sigma' must be positive")
		}
	unif=runif(n,0,1)
	sigma*sqrt(-2*log1p(-unif))
	##################################################
	##################################################
	}
