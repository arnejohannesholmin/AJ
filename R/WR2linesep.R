#*********************************************
#*********************************************
#' Read the tournaments as saved from copy paste from the WR, and save to a file with the following structure:
#' NA
#' Tournament name
#' Date
#' Series
#' Number of players in A
#' Player_1
#' Player_2
#' ...
#' Player_k
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname WR2linesep
#'
WR2linesep=function(dir,dir2=NULL){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2012-09-22 - Clean version.
	########### DESCRIPTION: ###########
	# Read the tournaments as saved from copy paste from the WR, and save to a file with the following structure:
	# 
	# Tournament name
	# Date
	# Series
	# Number of players in A
	# Player_1
	# Player_2
	# ...
	# Player_k
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'dir' is the directory holding the tournament files.
	# - 'dir2' is the new directory of the lineseparated tournament files.
	
	
	##################################################
	##################################################
	##### Preparation #####
	# Set the directory of the lineseparated files, if missing:
	if(length(dir2)==0){
		dir2=paste(dir,"_Linesep",sep="")
		}
	# Create the new directory:
	dir.create(dir2)
	
	# Get the list of tournament file names and create the paths to the old and the new files:
	f=list.files(dir,full.names=FALSE,recursive=TRUE)
	oldf=file.path(dir,f)
	newf=file.path(dir2,paste(f,".txt",sep=""))
	
	
	##### Execution and output #####
	# Loop through the tournaments and write the new files:
	for(i in seq_along(oldf)){
		cat("Converting file nr ",i,": ",oldf[i],"\n",sep="")
		suppressWarnings(this<-readfromWR(oldf[i]))
		this=c(this$tournament,this$dates,this$series,this$A,this$names)
		writeLines(this,newf[i])
		}
	##################################################
	##################################################
	}
