#*********************************************
#*********************************************
#' Returns an array of randomly generated values from the hypergeometric distribution.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname Rhyper
#'
Rhyper=function(dim,m,n,k,byrow=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	########### DESCRIPTION: ###########
	# Returns an array of randomly generated values from the hypergeometric distribution.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'dim' is the dimension of the output array.
	# - 'm', 'n' and 'k' are from rhyper().
	# - 'byrow' is TRUE if a matrix is requested (length(dim)==2), and the values should be ordered by rows in the matrix. Useful when different expectations are needed for each column of a matrix.
	
	
	##################################################
	##################################################
	if(length(dim)==2 && byrow){
		matrix(rhyper(prod(dim),m,n,k),nrow=dim[1],ncol=dim[2],byrow=byrow)
		}
	else{
		array(rhyper(prod(dim),m,n,k),dim=dim)
		}
	##################################################
	##################################################
	}
