#*********************************************
#*********************************************
#' Writes all tournaments in the interval 't' (given as a two element vector c(yyyymmdd,yyyymmdd)) to one excel file.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD chars
#'
#' @export
#' @rdname writeTournaments
#'
writeTournaments<-function(t="all",file=NULL){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2012-01-04 - Clean version.
	########### DESCRIPTION: ###########
	# Writes all tournaments in the interval 't' (given as a two element vector c(yyyymmdd,yyyymmdd)) to one excel file.
	########## DEPENDENCIES: ###########
	# get.points_fantasyNBA()
	############ VARIABLES: ############
	# - 't' is a two element vector c(yyyymmdd,yyyymmdd) specifying the time period (including the end points).
	# - 'file' is the name of the file to be written. If missing, file="/Users/arnejohannesholmin/Documents/Diverse/Bordhockey/FantasyNBA/Tournaments excel/AllTournaments.xls".
	
	
	##################################################
	##################################################
	##### Preparation #####
	# All tournaments are located in the same directory, and read according to the date:
	dir="/Users/arnejohannesholmin/Documents/Diverse/Bordhockey/FantasyNBA/Tournaments"
	# Set the default file:
	if(length(file)==0){
		file="/Users/arnejohannesholmin/Documents/Diverse/Bordhockey/FantasyNBA/Handouts/AllTournaments.xls"
		}
	
	
	##### Execution #####
	# Get the points acheived using the parameters specified:
	datT=get.points_fantasyNBA(dir,pow=c(3,3,3,3,1,1),w=c(32,32,31,28,10,6),t=t)$tournaments
	datT=lapply(datT,function(x) cbind(Rank=as.numeric(x[,1]),x[,2:4],as.data.frame(apply(x[,c(5:8)],2,as.numeric))))
	# Order decreasing chronologically and add suited names:
	datT=datT[order(substr(names(datT),1,8),substr(names(datT),10,18),decreasing=TRUE)]
	names(datT)=gsub("/","-",names(datT),fixed=TRUE)
	names(datT)=paste(substr(names(datT),1,4),substr(names(datT),5,6),substring(names(datT),7),sep="-")
	
	
	##### Output #####
	# Write the tournaments:
	for(i in seq_along(datT)){
		write.table(array(c(names(datT)[i],"","","","","","",""),dim=c(1,8)),file,append=i>1,sep="\t",row.names=FALSE,col.names=FALSE)
		suppressWarnings(write.table(datT[[i]],file,append=TRUE,sep="\t",row.names=FALSE,dec=","))
		if(i<length(datT)){
			suppressWarnings(write.table(chars(1,8),file,append=TRUE,sep="\t",row.names=FALSE,col.names=FALSE))
			}
		}
	##################################################
	##################################################
	}
