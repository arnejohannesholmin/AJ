#*********************************************
#*********************************************
#' ***DECRIPTION MISSING***
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname getWR_2014
#'
getWR_2014<-function(players=c("all","nor+","nor"),date=NULL,file=NULL){
	
	# allplayers=getallplayers()
	addnames=readLines(file.path(Sys.getenv("HOME"),"Documents/Diverse/Bordhockey/FantasyNBA/Rankings/Added players"))
	
	if(length(file)==0){
		dir=file.path(Sys.getenv("HOME"),"Documents/Diverse/Bordhockey/FantasyNBA/Rankings_2014")
		f=list.files(dir,full.names=TRUE)
		rankingfiles=f[substr(basename(f),1,7)=="ranking"]
		rankingdate=as.numeric(paste(substr(basename(rankingfiles),8,11),substr(basename(rankingfiles),13,14),substr(basename(rankingfiles),16,17),sep=""))
		if(length(date)){
			rankingdate[rankingdate>date]=-Inf
			file=rankingfiles[which.max(rankingdate)]
			cat("WR file read: ",file,"\n")
			}
		else{
			file=rankingfiles[which.max(rankingdate)]
			cat("The latest WR file read: ",file,"\n")
			}
		}

	
	##### Execution and output #####
	# Read the ranking file:
	dd=read.table(file,skip=1,header=TRUE,sep="\t",dec = ",")
	if(any(names(dd)=="Player_Value")){
		dd$Best=dd$Player_Value
		dd$Player_Value=NULL
		}
	# Get the total score, best score and the price calculated by adding par[2] to the total score times par[1], rounded to 0.1:
	dd$Points=as.numeric(dd$Points)
	dd$Best=round(as.numeric(dd$Best))
	
	if(tolower(players[1])=="nor"){
		players=tolower(dd[,5])=="nor"
		dd[players,]
		}
	else if(tolower(players[1])=="nor+"){
		players=tolower(dd[,5])=="nor" | dd[,5] %in% addnames
		dd[players,]
		}
	else if(tolower(players[1])=="all"){
		dd
		}
	else{
		players=dd[,5] %in% players
		dd[players,]
		}
	##################################################
	##################################################
	}
