#*********************************************
#*********************************************
#' Writes the function name, description and variables explainaiton of the functions given by 'functions' to the available examples files given by 'examples'. The example file must have the structure given by "/Applications/R/example template".
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom stats density
#'
#' @export
#' @rdname regroup_school
#'
regroup_school<-function(pos=NULL,len=1,adjust=3){
		
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2010-05-08 - Clean version.
	########### DESCRIPTION: ###########
	# Writes the function name, description and variables explainaiton of the functions given by 'functions' to the available examples files given by 'examples'. The example file must have the structure given by "/Applications/R/example template".
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'pos' is a string representing the name of a directory containing the positions to plot, if previously generated. If pos==NULL, positions are generated from the case.
	
	
	##################################################
	##################################################
	##### Preparation #####
	ntime=dim(pos)[3]
	framesize=c(-len/2,len/2)
	
	ngroups<-function(x,from=framesize[1],to=framesize[2]){
		ll=local.max(density(x,adjust=adjust,from=from,to=to))
		sum(ll[,2]>sqrt(.Machine$double.eps))
		}
		
	center_largest<-function(x,from,to){
		ll=local.max(density(x,adjust=adjust,from=from,to=to))
		ll=ll[ll[,2]>sqrt(.Machine$double.eps),,drop=FALSE]
		ll=ll[which.max(ll[,2]),1]
		ll
		}
	
	ngroupsx=which(apply(pos[1,,],2,ngroups)>1)
	ngroupsy=which(apply(pos[2,,],2,ngroups)>1)
	ngroupsz=which(apply(pos[3,,],2,ngroups)>1)
	
	
	##### Execution and output #####
	if(length(ngroupsx)>0){
		for(i in ngroupsx){
			cat(i,"\n")
			cmx1=center_largest(pos[1,,i],from=framesize[1],to=framesize[2])
			pos[1,,i]=pos[1,,i]-len*round((pos[1,,i]-cmx1)/len)
			}
		cat("\n")
		}
				
	if(length(ngroupsy)>0){
		for(i in ngroupsy){
			cat(i,"\n")
			cmy1=center_largest(pos[2,,i],from=framesize[1],to=framesize[2])
			pos[2,,i]=pos[2,,i]-len*round((pos[2,,i]-cmy1)/len)
			}
		cat("\n")
		}
	
	if(length(ngroupsz)>0){
		for(i in ngroupsz){
			cat(i,"\n")
			cmz1=center_largest(pos[3,,i],from=framesize[1],to=framesize[2])
			pos[3,,i]=pos[3,,i]-len*round((pos[3,,i]-cmz1)/len)
			}
		cat("\n")
		}
	
	pos
	##################################################
	##################################################
	}
