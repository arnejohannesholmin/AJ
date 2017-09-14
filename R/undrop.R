#*********************************************
#*********************************************
#' Adds dimensions to the input element that has length > 1, according to the other (1 or 2) input elements. 
#' NA
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom gdata left right
#' @importFrom TSD dim_all ones
#'
#' @export
#' @rdname undrop
#'
undrop<-function(...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-02-15 - Finished.
	# Last: 2009-07-06 - New method having '...' as input instead of 'left', 'x' and 'right'.
	########### DESCRIPTION: ###########
	# Adds dimensions to the input element that has length > 1, according to the other (1 or 2) input elements. 
	#	If no inputs are given, NULL is returned. 
	#	If 1 input is given, this is returned.
	#	If 2 inputs are given, and the first has length > 1 and the second has length == 1, the first input is returned with a number of dimensions added to the right according to the second input. Else the second input is returned with a number of dimensions added to the left according to the (first element of the) second input.
	#	If 3 or more inputs are given, the second is returned, with a number of dimensions added to the left and to the right according to the (first element of the) first input and the (first element of the) third input.
	########## DEPENDENCIES: ###########
	# dim_all(), ones()
	############ VARIABLES: ############
	# - '...' is the inputs as described in the DESCRIPTION.
		
	
	##################################################
	##################################################
	##### Preparation #####
	inlist=list(...)
	l=length(inlist)
	
	
	##### Execution and output #####
	if(l>2){
		left=ones(inlist[[1]][1])
		output=inlist[[2]]
		right=ones(inlist[[3]][1])
		dim(output)=c(left,dim_all(output,null.out=NULL),right)
		output
		}
	else if(l==2){
		l1=length(inlist[[1]])
		l2=length(inlist[[2]])
		if(l1>1 && l2==1){
			left=NULL
			output=inlist[[1]]
			right=ones(inlist[[2]][1])
			}
		else{
			left=ones(inlist[[1]][1])
			output=inlist[[2]]
			right=NULL
			}
		dim(output)=c(left,dim_all(output,null.out=NULL),right)
		output
		}	
	else if(l==1){
		inlist[[1]]
		}
	##################################################
	##################################################
	}
