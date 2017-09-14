#*********************************************
#*********************************************
#' Returns an array of randomly generated values from the negative binomial distribution.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname Rnbinom
#'
Rnbinom=function(dim,size,prob,mu,byrow=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	########### DESCRIPTION: ###########
	# Returns an array of randomly generated values from the negative binomial distribution.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'dim' is the dimension of the output array.
	# - 'size', 'prob' and 'mu' are from rnbinom().
	# - 'byrow' is TRUE if a matrix is requested (length(dim)==2), and the values should be ordered by rows in the matrix. Useful when different expectations are needed for each column of a matrix.
	
	
	##################################################
	##################################################
	if(length(dim)==2 && byrow){
		matrix(rnbinom(prod(dim),size,prob,mu),nrow=dim[1],ncol=dim[2],byrow=byrow)
		}
	else{
		array(rnbinom(prod(dim),size,prob,mu),dim=dim)
		}
	##################################################
	##################################################
	}
