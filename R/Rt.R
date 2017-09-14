#*********************************************
#*********************************************
#' Returns an array of randomly generated values from the student t distribution.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname Rt
#'
Rt=function(dim,df,ncp,byrow=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	########### DESCRIPTION: ###########
	# Returns an array of randomly generated values from the student t distribution.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'dim' is the dimension of the output array.
	# - 'df' and 'ncp' are from rt().
	# - 'byrow' is TRUE if a matrix is requested (length(dim)==2), and the values should be ordered by rows in the matrix. Useful when different expectations are needed for each column of a matrix.
	
	
	##################################################
	##################################################
	if(length(dim)==2 && byrow){
		matrix(rt(prod(dim),df,ncp),nrow=dim[1],ncol=dim[2],byrow=byrow)
		}
	else{
		array(rt(prod(dim),df,ncp),dim=dim)
		}
	##################################################
	##################################################
	}
