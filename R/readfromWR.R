#*********************************************
#*********************************************
#' Reads the data in copy-paste files from the world ranking in table hockey.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname readfromWR
#'
readfromWR<-function(x,nsign=10){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-12-20 - Clean version.
	########### DESCRIPTION: ###########
	# Reads the data in copy-paste files from the world ranking in table hockey.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'x' is the file to read.
	
	
	##################################################
	##################################################
	##### Preparation and execution #####
	# Read the lines of the file:
	l=readLines(x)
	start=substring(l,1,4)
	
	# Identify the postitions of the date, series and ranking:
	atdate=which(start=="Date")
	atseries=which(start=="Seri")
	at1=which(start=="Pos.")+1
	at2=which(start=="rank")-1
	
	# Extract the date:
	dates=strsplit(l[atdate],"\t")[[1]][2]
	dates=rev(strsplit(dates,".",fixed=TRUE)[[1]])
	dates[nchar(dates)==1]=paste("0",dates[nchar(dates)==1],sep="")
	dates=paste(dates,collapse="")
	
	# Get the series:
	series=strsplit(l[atseries],"\t")[[1]][2]
	
	# Get the type and name of tournament:
	type=substr(l[atdate-1],1,nsign)
	tournament=l[atdate-1]
	
	# Get the ranks, names, club names, nations and points of the players:
	s=strsplit(l[at1:at2],"\t")
	ranks=as.numeric(sapply(s,function(y) y[1]))
	names=sapply(s,function(y) y[4])
	clubs=sapply(s,function(y) y[5])
	nations=sapply(s,function(y) y[6])
	points=as.numeric(sapply(s,function(y) y[7]))
	
	# Get the number of players in A:
	suppressWarnings(A<-as.numeric(substr(x, unlist(regexec("_A_",x))+3, nchar(x))))
	A[is.na(A)]=1000
	
	
	##### Output #####
	# Return the list of information:
	return(list(tournament=tournament,dates=dates,names=names,clubs=clubs,nations=nations,points=points,ranks=ranks,type=type,series=series,A=A))
	##################################################
	##################################################
	}
