#*********************************************
#*********************************************
#' Transforms a numeric vector to a list of corresponding character arrays. If input has length=1, a vector is returned.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname num2abc
#'
num2abc=function(num,abc=letters[1:10]){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-02-24 - Finished.
	########### DESCRIPTION: ###########
	# Transforms a numeric vector to a list of corresponding character arrays. If input has length=1, a vector is returned.
	########## DEPENDENCIES: ###########
	# intseq()
	############ VARIABLES: ############
	# - 'num' is the input numerical vector.
	# - 'abc' is the sequence of letters corresponding to the integers.
	
	
	##################################################
	##################################################
	##### Preparation #####
	l=length(num)
	abc=c(abc[1:10],".")
	
	
	##### Execution and output #####
	# If input has length=1, a vector is returned:
	if(l==1){
		singlenum=intseq(num)
		singlenum=replace(singlenum,is.na(singlenum),11)
		return(abc[singlenum])
		}
	# intseq() splits numbers into single integer vectors:
	singlenum=intseq(num)
	out=vector("list", l)
	for(i in 1:l){
		# 0 is replaced by labc which has default 26:
		singlenumi=replace(singlenum[[i]],is.na(singlenum[[i]]),11)
		singlenumi=replace(singlenumi,singlenumi==0,10)
		out[[i]]=abc[singlenumi]
		}
	out
	##################################################
	##################################################
	}
