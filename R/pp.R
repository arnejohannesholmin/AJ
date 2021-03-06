#*********************************************
#*********************************************
#' Easing par(mfrow).
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname pp
#'
pp<-function(a=1,b=1,...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# Norsk
	############### LOG: ###############
	# Start: 2009-02-17 - Finished.
	########### DESCRIPTION: ###########
	# Easing par(mfrow).
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'a' and 'b' are the inputs in mfrow.
	# - '...' are variables passed on to par().
	
	
	##################################################
	##################################################
	if(length(a)>1){
		b=a[2]
		a=a[1]
		}
	par(mfrow=c(a,b),...)	
	##################################################
	##################################################
	}
