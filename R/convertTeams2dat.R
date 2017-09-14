#*********************************************
#*********************************************
#' Reads all teams in the given directory and writes to a table to be read more quickly later.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom gdata read.xls
#' @importFrom TSD chars
#'
#' @export
#' @rdname convertTeams2dat
#'
convertTeams2dat<-function(dir){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2012-01-01 - Clean version.
	########### DESCRIPTION: ###########
	# Reads all teams in the given directory and writes to a table to be read more quickly later.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'dir' is the directory holding the teams.
	
	
	##################################################
	##################################################
	f=list.files(dir,full.names=TRUE)
	xlsFiles=substring(f,sapply(gregexpr(".",f,fixed=TRUE),tail,1)+1) %in% c("xls","xlsx")
	f=f[xlsFiles]
	
	if(length(f)>0){
		a=sapply(gregexpr("@",basename(f),fixed=TRUE),head,2)
		teamnames=paste(substr(basename(f),1,a[1,]-1),substr(basename(f),a[1,]+1,a[2,]-1),sep=" - ")
		
		teams=chars(8,length(f))
		for(i in seq_along(f)){
			a=read.xls(f[i])
			teams[,i]=as.character(a$Team[1:8])
			}
		teams=rbind(teamnames,teams)
		
		write.table(teams,paste(dir,"AllTeams.dat",sep="/"),sep="\t",col.names=FALSE,row.names=FALSE)
		}
	##################################################
	##################################################
	}
