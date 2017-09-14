#*********************************************
#*********************************************
#' Extracts all but the file extensions from a vector of file names (discarding the dot by default).
#'
#' @param filelist  is a string vector of file names, from which the file extension is extracted.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname get.filename
#'
get.filename<-function(filelist, includedot=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2012-09-11 - Clean version.
	########### DESCRIPTION: ###########
	# Extracts all but the file extensions from a vector of file names (discarding the dot by default).
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---filelist--- is a string vector of file names, from which the file extension is extracted.
		

	##################################################
	##################################################
	paste0(unlist(lapply(strsplit(filelist,".",fixed=TRUE),function(x) x[-length(x)])), if(includedot) ".")
	##################################################
	##################################################
	}
