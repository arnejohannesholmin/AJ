#*********************************************
#*********************************************
#' Returns an array of randomly generated values from the chi-squared distribution.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname Rchisq
#'
Rchisq=function(dim,df,ncp=0,byrow=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	########### DESCRIPTION: ###########
	# Returns an array of randomly generated values from the chi-squared distribution.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'dim' is the dimension of the output array.
	# - 'df' and 'ncp' are from rchisq().
	# - 'byrow' is TRUE if a matrix is requested (length(dim)==2), and the values should be ordered by rows in the matrix. Useful when different expectations are needed for each column of a matrix.
	
	
	##################################################
	##################################################
	if(length(dim)==2 && byrow){
		matrix(rchisq(prod(dim),df,ncp),nrow=dim[1],ncol=dim[2],byrow=byrow)
		}
	else{
		array(rchisq(prod(dim),df,ncp),dim=dim)
		}
	##################################################
	##################################################
	}
