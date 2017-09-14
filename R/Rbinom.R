#*********************************************
#*********************************************
#' Returns an array of randomly generated values from the binomial distribution.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname Rbinom
#'
Rbinom=function(dim,size,prob,byrow=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	########### DESCRIPTION: ###########
	# Returns an array of randomly generated values from the binomial distribution.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'dim' is the dimension of the output array.
	# - 'size' and 'prob' are from rbinom().
	# - 'byrow' is TRUE if a matrix is requested (length(dim)==2), and the values should be ordered by rows in the matrix. Useful when different expectations are needed for each column of a matrix.
	
	
	##################################################
	##################################################
	if(length(dim)==2 && byrow){
		matrix(rbinom(prod(dim),size,prob),nrow=dim[1],ncol=dim[2],byrow=byrow)
		}
	else{
		array(rbinom(prod(dim),size,prob),dim=dim)
		}
	##################################################
	##################################################
	}
