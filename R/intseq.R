#*********************************************
#*********************************************
#' Transforms a number to a vector of single intergers. Commas returned as NA. Very slow function.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname intseq
#'
intseq=function(num){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-02-24 - Finished.
	########### DESCRIPTION: ###########
	# Transforms a number to a vector of single intergers. Commas returned as NA. Very slow function.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'num' is the input number or vector of numbers.
	
	
	##################################################
	##################################################
	l=length(num)
	if(l==1){
		suppressWarnings(as.numeric(strsplit(as.character(num),"")[[1]]))
		}
	else{
		out=list()
		for(i in 1:l){
			suppressWarnings(out[[i]]<-as.numeric(strsplit(as.character(num),"")[[i]]))
			}
		out
		}
	##################################################
	##################################################
	}
