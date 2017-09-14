#*********************************************
#*********************************************
#' Returns an array of randomly generated values from the gamma distribution.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname Rgamma
#'
Rgamma=function(dim,shape,rate=1,scale=1/rate,byrow=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	########### DESCRIPTION: ###########
	# Returns an array of randomly generated values from the gamma distribution.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'dim' is the dimension of the output array.
	# - 'shape', 'rate' and 'scale' are from rgamma().
	# - 'byrow' is TRUE if a matrix is requested (length(dim)==2), and the values should be ordered by rows in the matrix. Useful when different expectations are needed for each column of a matrix.
	
	
	##################################################
	##################################################
	if(length(dim)==2 && byrow){
		matrix(rgamma(prod(dim),shape,rate,scale),nrow=dim[1],ncol=dim[2],byrow=byrow)
		}
	else{
		array(rgamma(prod(dim),shape,rate,scale),dim=dim)
		}
	##################################################
	##################################################
	}
