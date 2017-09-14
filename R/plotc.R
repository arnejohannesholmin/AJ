#*********************************************
#*********************************************
#' To aviod typing 'type="c"' all the time.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname plotc
#'
plotc<-function(...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# Norsk
	############### LOG: ###############
	# Start: 2008-05-31 - First version without ... in the parameter list.
	# Last:  2009-05-05 - Removed the item y=NULL in the parameter list (which was unneccesary).
	########### DESCRIPTION: ###########
	# To aviod typing 'type="c"' all the time.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - '...' see plot.default
	
	
	##################################################
	##################################################
	plot(...,type="c")
	##################################################
	##################################################
	}
