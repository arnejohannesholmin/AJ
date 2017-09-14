#*********************************************
#*********************************************
#' Locating elements equal to 'item' in a list.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname lwhich
#'
lwhich<-function(x,item=NULL,fun=function(x) x,nomatch=NULL,robust=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-03 - Finished.
	########### DESCRIPTION: ###########
	# Locating elements equal to 'item' in a list.
	############ VARIABLES: ############
	# - 'x' in the input list.
	# - 'item' is the item to locate in the list.
	# - 'fun' is a function to perform on the elements of the list.
	# - 'nomatch' is the value to return if no matches are found.
	# - 'robust' is TRUE if the more time demanding comparison is used and FALSE if identical() is used (see "Preparation").
	
	
	##################################################
	##################################################
	##### Preparation #####
	if(!is.list(x)){
		x=as.list(x)
		}
	l=length(x)
	if(robust){
		is.item=function(x){
			if(identical(length(x),length(item))){
				identical(fun(x),item) | (all(fun(x)==item) & !any(is.null(fun(x)),is.null(item),identical(fun(x),logical(0)),identical(item,logical(0))))
				}
			else{
				FALSE
				}
			}
		}
	else{
		is.item=function(x){
			identical(fun(x),item)
			}
		}
	
	
	##### Execution and output #####
	out=unlist(lapply(x,is.item))
	if(sum(out)==0){
		return(nomatch)
		}
	return((1:l)[out])
	##################################################
	##################################################
	}
