#*********************************************
#*********************************************
#' Returns an array of randomly generated values from the logistic distribution.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname Rlogis
#'
Rlogis=function(dim,location=0,scale=1,byrow=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	########### DESCRIPTION: ###########
	# Returns an array of randomly generated values from the logistic distribution.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'dim' is the dimension of the output array.
	# - 'location' and 'scale' are from rlogis().
	# - 'byrow' is TRUE if a matrix is requested (length(dim)==2), and the values should be ordered by rows in the matrix. Useful when different expectations are needed for each column of a matrix.
	
	
	##################################################
	##################################################
	if(length(dim)==2 && byrow){
		matrix(rlogis(prod(dim),location,scale),nrow=dim[1],ncol=dim[2],byrow=byrow)
		}
	else{
		array(rlogis(prod(dim),location,scale),dim=dim)
		}
	##################################################
	##################################################
	}
