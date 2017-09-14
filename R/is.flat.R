#*********************************************
#*********************************************
#' Checks for equality of all elements in an array given the tolerance.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname is.flat
#'
is.flat<-function(x,tol=0){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2010-05-08 - Clean version.
	########### DESCRIPTION: ###########
	# Checks for equality of all elements in an array given the tolerance.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'x' is a vector, array or list.
	# - 'tol' is the tolerance of the equality requirement.
	
	
	##################################################
	##################################################
	diff(range(x)) <= tol
	##################################################
	##################################################
	}
