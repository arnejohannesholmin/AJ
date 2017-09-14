#*********************************************
#*********************************************
#' Returns an array of randomly generated values from the F distribution.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname Rf
#'
Rf=function(dim,df1,df2,ncp,byrow=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	########### DESCRIPTION: ###########
	# Returns an array of randomly generated values from the F distribution.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'dim' is the dimension of the output array.
	# - 'df1', 'df2' and 'ncp' are from rf().
	# - 'byrow' is TRUE if a matrix is requested (length(dim)==2), and the values should be ordered by rows in the matrix. Useful when different expectations are needed for each column of a matrix.
	
	
	##################################################
	##################################################
	if(length(dim)==2 && byrow){
		matrix(rf(prod(dim),df1,df2,ncp),nrow=dim[1],ncol=dim[2],byrow=byrow)
		}
	else{
		array(rf(prod(dim),df1,df2,ncp),dim=dim)
		}
	##################################################
	##################################################
	}
