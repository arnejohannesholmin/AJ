#*********************************************
#*********************************************
#' Returns an array of randomly generated values from the Wilcoxon distribution.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname Rwilcox
#'
Rwilcox=function(dim,m,n,byrow=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	# Last: 2010-09-28 - Added the option 'byrow'.
	########### DESCRIPTION: ###########
	# Returns an array of randomly generated values from the Wilcoxon distribution.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'dim' is the dimension of the output array.
	# - 'n' and 'm' are from rwilcox().
	# - 'byrow' is TRUE if a matrix is requested (length(dim)==2), and the values should be ordered by rows in the matrix. Useful when different expectations are needed for each column of a matrix.
	
	
	##################################################
	##################################################
	if(length(dim)==2 && byrow){
		matrix(rwilcox(prod(dim),m,n),nrow=dim[1],ncol=dim[2],byrow=byrow)
		}
	else{
		array(rwilcox(prod(dim),m,n),dim=dim)
		}
	##################################################
	##################################################
	}
