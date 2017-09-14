#*********************************************
#*********************************************
#' Reads all teams in the given directory using the quickly accessed file "AllTeams.dat".
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname readTeams
#'
readTeams=function(dir){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2012-01-01 - Clean version.
	########### DESCRIPTION: ###########
	# Reads all teams in the given directory using the quickly accessed file "AllTeams.dat".
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'dir' is the directory holding the teams.
	
	
	##################################################
	##################################################
	filename=paste(dir,"AllTeams.dat",sep="/")
	if(file.exists(filename)){
		teams=as.matrix(read.table(filename,sep="\t"))
		colnames(teams)=teams[1,]
		# Output:
		teams[-1,]
		}
	##################################################
	##################################################
	}
