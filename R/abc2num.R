#*********************************************
#*********************************************
#' Transforms a vector of strings to a list of numeric vectors where "a" corresponds to 1, "b" corresponds to 2 and so on. If 'x' has length=1 a vector is returned.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname abc2num
#'
abc2num=function(x,char=letters,ignorecase=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-02-24 - Finished.
	# Last: 2009-07-27 - Changed method from doing two for loops, to using the match() function. Added a second input parameter specifying the character vector used as the reference for the matching.
	########### DESCRIPTION: ###########
	# Transforms a vector of strings to a list of numeric vectors where "a" corresponds to 1, "b" corresponds to 2 and so on. If 'x' has length=1 a vector is returned.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'x' is the input string vector to be transformed to a list of vectors of corresponding indexes of the reference character vector 'char'.
	# - 'char' is the reference character vector.
	# - 'ignorecase' is TRUE if case is to be ignored. If FALSE 'char' should contain capital letters.
	
	
	##################################################
	##################################################
	##### Preparation #####
	lx=length(x)
	if(ignorecase){
		x=tolower(x)
		}
	# Splits the input into single characters:
	x=strsplit(x,"")
	
	
	##### Execution and output #####
	out=vector("list", lx)
	for(i in 1:lx){
		out[[i]]=match(x[[i]],char)
		}
	if(lx==1){
		return(out[[1]])
		}
	else{
		return(out)
		}
	##################################################
	##################################################
	}
