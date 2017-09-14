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
#' @rdname getpriceWR
#'
getpriceWR<-function(file=NULL,date=NULL,par=c(0.01,2/3)){
	
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
	##### Preparation #####
	allplayers=getallplayers()
	
	if(length(file)==0){
		dir="/Users/arnejohannesholmin/Documents/Diverse/Bordhockey/FantasyNBA/Rankings"
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

	
	##### Execution #####
	# Read the ranking file:
	dd=read.table(file,skip=1,header=TRUE,sep="\t")
	# Get the total score, best score and the price calculated by adding par[2] to the total score times par[1], rounded to 0.1:
	dd$Points=as.numeric(dd$Points)
	dd$Best=as.numeric(dd$Best)
	dd$price0 = round(par[1] * dd$Points  + par[2],digits=1)
	
	
	##### Output #####
	# Construct the data frame to return and extract the norwegian players and the approved players given in 'addnames':
	dat=data.frame(Player=levels(dd$Player)[dd$Player],Club=levels(dd$Club)[dd$Club],Nation=levels(dd$Nation)[dd$Nation],PointsWR=dd$Points,Best=dd$Best,PriceWR=dd$price0)
	dat[,1]=levels(dat[,1])[dat[,1]]
	# Match the players to the 'allplayers':
	matchWR=match(allplayers[,1],dat[,1])
	matchWR=matchWR[!is.na(matchWR)]
	# Insert the WR-data:
	out=cbind(as.data.frame(allplayers),PointsWR=0,Best=0,PriceWR=round(par[2],digits=1))
	out[allplayers[,1] %in% dat[,1],]=dat[matchWR,]
	
	
	# Return:
	out
	##################################################
	##################################################
	}
