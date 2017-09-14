#*********************************************
#*********************************************
#' Variance of an exponential distribution restricted by X<C or X>C.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname var.condexp
#'
var.condexp<-function(beta,C,type="l"){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2010-03-12 - Clean version.
	########### DESCRIPTION: ###########
	# Variance of an exponential distribution restricted by X<C or X>C.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'beta' is the parameter in the exponential distribution defined as 1/beta*exp(-x/beta).
	# - 'C' cut point defineng the conditional distribution.
	# - 'type' is "l" if 'mean' is the mean of X|X<C and "g" if 'mean' is the mean of X|X>C.
		
	
	##################################################
	##################################################
	e=-C/beta
	Fc=1-exp(e)
	(-exp(e)*(C^2+2*beta*C+2*beta^2)+2*beta^2)/Fc - ((beta-(C+beta)*exp(e))/Fc)^2
	##################################################
	##################################################
	}
