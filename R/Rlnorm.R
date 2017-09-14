#*********************************************
#*********************************************
#' Returns an array of randomly generated values from the lognormal distribution.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname Rlnorm
#'
Rlnorm=function(dim,meanlog=0,sdlog=1,byrow=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	########### DESCRIPTION: ###########
	# Returns an array of randomly generated values from the lognormal distribution.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'dim' is the dimension of the output array.
	# - 'meanlog' and 'sdlog' are from rlnorm().
	# - 'byrow' is TRUE if a matrix is requested (length(dim)==2), and the values should be ordered by rows in the matrix. Useful when different expectations are needed for each column of a matrix.
	
	
	##################################################
	##################################################
	if(length(dim)==2 && byrow){
		matrix(rlnorm(prod(dim),meanlog,sdlog),nrow=dim[1],ncol=dim[2],byrow=byrow)
		}
	else{
		array(rlnorm(prod(dim),meanlog,sdlog),dim=dim)
		}
	##################################################
	##################################################
	}
