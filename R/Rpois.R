#*********************************************
#*********************************************
#' Returns an array of randomly generated values from the Poisson distribution.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname Rpois
#'
Rpois=function(dim,lambda,byrow=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	########### DESCRIPTION: ###########
	# Returns an array of randomly generated values from the Poisson distribution.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'dim' is the dimension of the output array.
	# - 'lambda' is from rpois().
	# - 'byrow' is TRUE if a matrix is requested (length(dim)==2), and the values should be ordered by rows in the matrix. Useful when different expectations are needed for each column of a matrix.
	
	
	##################################################
	##################################################
	if(length(dim)==2 && byrow){
		matrix(rpois(prod(dim),lambda),nrow=dim[1],ncol=dim[2],byrow=byrow)
		}
	else{
		array(rpois(prod(dim),lambda),dim=dim)
		}
	##################################################
	##################################################
	}
