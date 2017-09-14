#*********************************************
#*********************************************
#' Returns an array of randomly generated values from the beta distribution.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom stats rbeta
#'
#' @export
#' @rdname Rbeta
#'
Rbeta=function(dim,shape1,shape2,ncp=0,byrow=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	########### DESCRIPTION: ###########
	# Returns an array of randomly generated values from the beta distribution.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'dim' is the dimension of the output array.
	# - 'shape1', 'shape1' and 'ncp' are from rbeta().
	# - 'byrow' is TRUE if a matrix is requested (length(dim)==2), and the values should be ordered by rows in the matrix. Useful when different expectations are needed for each column of a matrix.
	
	
	##################################################
	##################################################
	if(length(dim)==2 && byrow){
		matrix(rbeta(prod(dim),shape1,shape2,ncp),nrow=dim[1],ncol=dim[2],byrow=byrow)
		}
	else{
		array(rbeta(prod(dim),shape1,shape2,ncp),dim=dim)
		}
	##################################################
	##################################################
	}
