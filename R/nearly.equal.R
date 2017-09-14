#*********************************************
#*********************************************
#' Tests for near equality. To be used when == fails to equal values that are intended to be equal, due to machine inaccuracy. See info(nearly.equal). Related to all.equal() but returns a logical vector instead of only one logical.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname nearly.equal
#'
nearly.equal<-function(x,y,tolerance=.Machine$double.eps^0.5){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-09-20 - Clean version.
	########### DESCRIPTION: ###########
	# Tests for near equality. To be used when == fails to equal values that are intended to be equal, due to machine inaccuracy. See info(nearly.equal). Related to all.equal() but returns a logical vector instead of only one logical.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'x' and 'y' are the R-objects to compare. If the mode of 'x' and 'y' is character, simple == is done. If the mode of 'x' and 'y' is numeric, equality is checked against the value of 'tolerance'. Else identical() is used.
	# - 'tolerance' is the tolerance of the comparison.
	
	
	##################################################
	##################################################
	if(all(is.character(x),is.character(y))){
		x==y
		}
	else if(all(is.numeric(x),is.numeric(y))){
		abs(x-y)<tolerance
		}
	else{
		identical(x,y)
		}
	##################################################
	##################################################
	}
