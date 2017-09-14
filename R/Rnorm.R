#*********************************************
#*********************************************
#' Returns an array of randomly generated values from the Gaussian distribution.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom stats rnorm
#'
#' @export
#' @rdname Rnorm
#'
Rnorm=function(dim,mean=0,sd=1,byrow=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	########### DESCRIPTION: ###########
	# Returns an array of randomly generated values from the Gaussian distribution.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'dim' is the dimension of the output array.
	# - 'mean' and 'sd' are from rnorm().
	# - 'byrow' is TRUE if a matrix is requested (length(dim)==2), and the values should be ordered by rows in the matrix. Useful when different expectations are needed for each column of a matrix.
	
	
	##################################################
	##################################################
	if(length(dim)==2 && byrow){
		matrix(rnorm(prod(dim),mean,sd),nrow=dim[1],ncol=dim[2],byrow=byrow)
		}
	else{
		array(rnorm(prod(dim),mean,sd),dim=dim)
		}
	##################################################
	##################################################
	}
