#*********************************************
#*********************************************
#' Creates a vector of strings holding the integers given by 'int' added zeros to the left so that all strings have length 'len'.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD zeros
#'
#' @export
#' @rdname int2str
#'
int2str<-function(int,len=0){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-08-31 - Clean version.
	########### DESCRIPTION: ###########
	# Creates a vector of strings holding the integers given by 'int' added zeros to the left so that all strings have length 'len'.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'int' is a vector of integers from which the string sequence is to be constructed.
	# - 'len' is the (non-negative) number of characters of the strings (Defaulted to 0, which assures no added zeros).
	
	
	##################################################
	##################################################
	# Constructing the zeros prior to the integers:
	zeros=rep(paste(double(len),collapse=""),length(int))
	zeros=substr(zeros,1,len-nchar(int))
	# Pasting to the output:
	paste(zeros,int,sep="")
	##################################################
	##################################################
	}
