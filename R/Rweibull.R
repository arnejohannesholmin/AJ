#*********************************************
#*********************************************
#' Returns an array of randomly generated values from the Weibull distribution.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname Rweibull
#'
Rweibull=function(dim,shape,scale=1,byrow=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	########### DESCRIPTION: ###########
	# Returns an array of randomly generated values from the Weibull distribution.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'dim' is the dimension of the output array.
	# - 'shape' and 'scale' are from rweibull().
	# - 'byrow' is TRUE if a matrix is requested (length(dim)==2), and the values should be ordered by rows in the matrix. Useful when different expectations are needed for each column of a matrix.
	
	
	##################################################
	##################################################
	if(length(dim)==2 && byrow){
		matrix(rweibull(prod(dim),shape,scale),nrow=dim[1],ncol=dim[2],byrow=byrow)
		}
	else{
		array(rweibull(prod(dim),shape,scale),dim=dim)
		}
	##################################################
	##################################################
	}
