#*********************************************
#*********************************************
#' Extracts a subset of 3D data given by 'data', according to the array subset 'subset' or the cartesian subset 'range'.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname rm.whsp
#'
rm.whsp<-function(x,keep.lsp=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2010-03-23 - Clean version.
	########### DESCRIPTION: ###########
	# Extracts a subset of 3D data given by 'data', according to the array subset 'subset' or the cartesian subset 'range'.
	########## DEPENDENCIES: ###########
	# extract()
	############ VARIABLES: ############
	# - 'x' is an R object to strip of white spaces.
	# - 'keep.lsp' is true if line spaces are to be kept.
		
	
	##################################################
	##################################################
	x=gsub(" ","",x)
	x=gsub("\t","",x)
	if(!keep.lsp){
		x=gsub("\r","",x)
		x=gsub("\n","",x)
		}
	x
	##################################################
	##################################################
	}
