#*********************************************
#*********************************************
#' Removing elements equal to 'item' from a list.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname lremove
#'
lremove<-function(x,item=NULL,robust=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-03-03 - Finished.
	# Last: 2009-07-31 - Added support.
	########### DESCRIPTION: ###########
	# Removing elements equal to 'item' from a list.
	############ VARIABLES: ############
	# - 'x' in the input list.
	# - 'item' is the item to remove from the list.
	# - 'robust' is TRUE if the more time demanding comparison is used and FALSE if identical() is used (see "Preparation").
	
	
	##################################################
	##################################################
	##### Preparation #####
	if(!is.list(x)){
		x=as.list(x)
		}
	l=length(x)
	if(robust){
		isnot.item=function(x){
			if(identical(length(x),length(item))){
				!(identical(x,item) | (all(x==item) & !any(is.null(x),is.null(item),identical(x,logical(0)),identical(item,logical(0)))))
				}
			else{
				TRUE
				}
			}
		}
	else{
		isnot.item=function(x){
			!identical(x,item)
			}
		}
	
	
	##### Execution and output #####
	out=lapply(x,isnot.item)
	x[unlist(out)]
	##################################################
	##################################################
	}
