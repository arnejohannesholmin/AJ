#*********************************************
#*********************************************
#' Returns the unique values of the R object 'x', where nearly equal values are regarded as equal.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname nearly.unique
#'
nearly.unique<-function(x,tolerance=.Machine$double.eps^0.5){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-09-20 - Clean version.
	########### DESCRIPTION: ###########
	# Returns the unique values of the R object 'x', where nearly equal values are regarded as equal.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'x' is the R-object to unique. If not numeric, ordinary unique() is used.
	# - 'tolerance' is the tolerance defining equal elements.
	
	
	##################################################
	##################################################
	##### Preparation #####
	# Using the build in unique() function:
	u=unique(x)
		
	
	##### Execution and output #####
	if(is.numeric(x)){
		# If 'u' is not ordered, special care needs to be taken, as the action "abs(u[-1]-u[-length(u)])>tolerance" assumes ordered values:
		if(is.unsorted(u)){
			o=order(u)
			uo=u[o]
			e=c(FALSE,abs(uo[-1]-uo[-length(u)])>tolerance)
			# Adding the first element of 'u' to the output:
			c(u[1],u[e[o]])
			}
		else{
			e=c(TRUE,abs(u[-1]-u[-length(u)])>tolerance)
			u[e]
			}
		}
	else{
		u
		}
	##################################################
	##################################################
	}
