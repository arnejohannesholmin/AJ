#*********************************************
#*********************************************
#' Flattens lists so that all R-objects to the desired depth in the input list 'x' are placed in separate positions in the output list. Differs from unlist() by the fact that unlist() flattens an array to single elements.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom grid depth
#'
#' @export
#' @rdname flatten.list
#'
flatten.list<-function(x,depth=Inf){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-09-15 - Clean version.
	########### DESCRIPTION: ###########
	# Flattens lists so that all R-objects to the desired depth in the input list 'x' are placed in separate positions in the output list. Differs from unlist() by the fact that unlist() flattens an array to single elements.
	########## DEPENDENCIES: ###########
	#	
	############ VARIABLES: ############
	########### DESCRIPTION: ###########
	# - 'x' is a list to be flattened so that all R-objects are placed the resulting list of one level.
	# - 'depth' is the number of recursive levels in the list for which objects are flattened.
	
	
	##################################################
	##################################################
	i=0
	while(any(sapply(x,is.list))){
		# Stop if the desired depth is reached or the list is flat:
		if(i>=depth){
			return(x)
			}
		i=i+1
		# Identify the elements that are lists:
		arelists=sapply(x,is.list)
		# Get the number of elements of the list objects:
		nelements=sapply(x,function(x) if(is.list(x)) length(x) else 1)
		# The new and longer list to which the elements of the list-objects of 'x' are to be inserted:
		ll=vector("list",sum(nelements))
		# Get the indexes of the new list in which to insert:
		indl=rep(seq_along(x),nelements)
		indl_list=indl %in% seq_along(x)[arelists]
		indl_notlist=indl %in% seq_along(x)[!arelists]
		# Insert the list elements and the non-list elements_
		ll[indl_notlist]=x[!arelists]
		ll[indl_list]=unlist(x[arelists],FALSE)
		x=ll
		}
	x
	##################################################
	##################################################
	}
