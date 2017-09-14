#*********************************************
#*********************************************
#' Prints the text of 'expr' prior to the printing of 'expr'.
#'
#' @param expr  is the expression to print.
#' @param ...  variables passed on to print().
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname printt
#'
printt<-function(expr=NULL,...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-08-25 - Clean version.
	########### DESCRIPTION: ###########
	# Prints the text of 'expr' prior to the printing of 'expr'.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---expr--- is the expression to print.
	# ---...--- variables passed on to print().
	
	
	##################################################
	##################################################
	print(deparse(substitute(expr)))
	print(expr,...)
	cat("\n")
	##################################################
	##################################################
	}
