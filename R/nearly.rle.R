#*********************************************
#*********************************************
#' Computes the lengths and values of runs of nearly equal values in a vector.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname nearly.rle
#'
nearly.rle<-function(x,tolerance=.Machine$double.eps^0.5){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-09-20 - Clean version.
	########### DESCRIPTION: ###########
	# Computes the lengths and values of runs of nearly equal values in a vector.
	########## DEPENDENCIES: ###########
	# nearly.duplicated()
	############ VARIABLES: ############
	# - 'x' is the R-object to repport for duplicates. If not numeric, ordinary duplicated() is used.
	# - 'tolerance' is the tolerance of the comparison.
	
	
	##################################################
	##################################################
	d=nearly.duplicated(x,tolerance=tolerance)
	# 'lengths' is found by considering the difference between consecutive indexes at which nearly.duplicated() has returned FALSE:
	lengths=diff(c(seq_along(x)[!d],length(x)+1))
	# 'values' are the values at the start of each sequence of nearly equal elements, found at indexes returned by nearly.duplicated():
	values=x[!d]
	# Print the result:
	list(lengths=lengths,values=values)
	##################################################
	##################################################
	}
