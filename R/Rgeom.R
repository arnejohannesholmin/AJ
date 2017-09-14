#*********************************************
#*********************************************
#' Returns an array of randomly generated values from the geometric distribution.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname Rgeom
#'
Rgeom=function(dim,prob,byrow=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	########### DESCRIPTION: ###########
	# Returns an array of randomly generated values from the geometric distribution.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'dim' is the dimension of the output array.
	# - 'prob' is from rgeom().
	# - 'byrow' is TRUE if a matrix is requested (length(dim)==2), and the values should be ordered by rows in the matrix. Useful when different expectations are needed for each column of a matrix.
	
	
	##################################################
	##################################################
	if(length(dim)==2 && byrow){
		matrix(rgeom(prod(dim),prob),nrow=dim[1],ncol=dim[2],byrow=byrow)
		}
	else{
		array(rgeom(prod(dim),prob),dim=dim)
		}
	##################################################
	##################################################
	}
