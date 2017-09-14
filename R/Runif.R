#*********************************************
#*********************************************
#' Returns an array of randomly generated values from the uniform distribution.
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
#' @rdname Runif
#'
Runif=function(dim,min=0,max=1,byrow=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	########### DESCRIPTION: ###########
	# Returns an array of randomly generated values from the uniform distribution.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'dim' is the dimension of the output array.
	# - 'min' and 'max' are from runif().
	# - 'byrow' is TRUE if a matrix is requested (length(dim)==2), and the values should be ordered by rows in the matrix. Useful when different expectations are needed for each column of a matrix.
	
	
	##################################################
	##################################################
	if(length(dim)==2 && byrow){
		matrix(runif(prod(dim),min,max),nrow=dim[1],ncol=dim[2],byrow=byrow)
		}
	else{
		array(runif(prod(dim),min,max),dim=dim)
		}
	##################################################
	##################################################
	}
