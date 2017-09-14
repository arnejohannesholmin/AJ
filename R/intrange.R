#*********************************************
#*********************************************
#' Pulls a range to the nearest grid point.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname intrange
#'
intrange<-function(a,int=1){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-02-17 - Finished.
	########### DESCRIPTION: ###########
	# Pulls a range to the nearest grid point.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'a' is an array.
	# - 'int' is the size of the grid to which the range is pulled.
	
	
	##################################################
	##################################################
	if(int==1){
		c(floor(min(a)),ceiling(max(a)))
		}
	else{
		c(floor(min(a)/int)*int,ceiling(max(a)/int)*int)
		}
	##################################################
	##################################################
	}
