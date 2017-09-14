#*********************************************
#*********************************************
#' Determines which elements of an R object are nearly equal the previous element. Differs from duplicated by 2 means: (1) The use of nearly equality. (2) The comparison to the prevoius element and not all previous elements.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname nearly.duplicated
#'
nearly.duplicated<-function(x,tolerance=.Machine$double.eps^0.5){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-09-20 - Clean version.
	########### DESCRIPTION: ###########
	# Determines which elements of an R object are nearly equal the previous element. Differs from duplicated by 2 means: (1) The use of nearly equality. (2) The comparison to the prevoius element and not all previous elements.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'x' is the R-object to repport for duplicates. If not numeric, ordinary duplicated() is used.
	# - 'tolerance' is the tolerance of the comparison.
	
	
	##################################################
	##################################################
	if(is.numeric(x)){
		c(FALSE,abs(x[-1]-x[-length(x)])<tolerance)
		}
	else{
		duplicated(x)
		}
	##################################################
	##################################################
	}
