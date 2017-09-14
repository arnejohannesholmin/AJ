#*********************************************
#*********************************************
#' Returns the dataframe of prices for all gameweeks, using the formula 2 + WR * w + (T + (P-G)*0.5) * s.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname getprice_testseason
#'
getprice_testseason<-function(date=NULL,sort=FALSE,volume="all"){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-12-20 - Clean version.
	########### DESCRIPTION: ###########
	# Returns the dataframe of prices for all gameweeks, using the formula 2 + WR * w + (T + (P-G)*0.5) * s.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	
	
	##################################################
	##################################################
	##### Preparation #####
	# Get all players:
	allplayers=getallplayers()
	# Read deadlines:
	t=as.matrix(read.table(file.path(Sys.getenv("HOME"),"Documents/Diverse/Bordhockey/FantasyNBA/Deadlines/Deadlines2012.dat"),header=TRUE)[,2:4])
	t[,3]=2
	lt2012Local=sum(t[,3]>0)
	lt2012NBA=sum(t[,3]==2)
	if(length(date) && nchar(date)==8){
		t=t[t[,1]<date,,drop=FALSE]
		}
	
	# All tournaments are located in the same directory, and read according to the date:
	dir=file.path(Sys.getenv("HOME"),"Documents/Diverse/Bordhockey/FantasyNBA/Tournaments")
	f=list.files(dir,full.names=TRUE)

	# Set the parameters of the prices:
	# Weighting between the WR and the points acheived last season:
	WRvsS=c(1,2) # c(1,2)
	WRvsS=WRvsS/sum(WRvsS)
	# Minimum price:
	minprice=2
	minpriceWR = minprice*WRvsS[1]
	minpriceS = minprice*WRvsS[2]
	# Parameters of the prices:
	ScaleWR=0.001
	ScaleS=0.025
	# Correct for half season:
	ScaleS=ScaleS*2
	
	
	##### Execution #####
	# Get the points acheived during 2011, using the parameters specified:
	total=get.points_fantasyNBA(dir,pow=c(3,3,3,3,1,1),w=c(32,32,31,28,10,6),t=c(20110101,20111231))$total
	# Correct for half season:
	total[,2:4]=total[,2:4]/2
	PriceS0=round(total$total*ScaleS+minpriceS,digits=1)
	
	# Get the prices of the players based on the world ranking:
	PriceWR0=getpriceWR(date=20111231,par=c(ScaleWR,minpriceWR))
	
	# Merge into the output:
	out=data.frame(Player=allplayers[,1],  Club=allplayers[,2],  Nation=allplayers[,3],  PointsWR0=PriceWR0$PointsWR,   BestWR0=PriceWR0$Best,  PriceWR0=PriceWR0$PriceWR,  Points2011=total$total,  MeanPointsNBA2011=total$totalNBA/lt2012NBA,  MeanPointsLocal2011=total$totalLocal/lt2012Local,  MeanPoints2011=total$total/lt2012Local,  PriceS0=PriceS0,  Price0=PriceWR0$PriceWR+PriceS0)
	printt(out[1:3,])
	# Define the round difference to the mean score:
	R=NULL
	# Get the points of each gameweek, and calculate the price:
	for(i in seq_len(nrow(t))){
		# Read the current WR-price and the gameweek points:
		printt(t[i,])
		thisPriceWR=getpriceWR(date=t[i,2],par=c(ScaleWR,minpriceWR))
		thistotal=get.points_fantasyNBA(dir,pow=c(3,3,3,3,1,1),w=c(32,32,31,28,10,6),t=t[i,],n=if(i==nrow(t)) volume else "all")$total
		
		# Use the formula given in the DESCRIPTION to get the total running price:
		if(t[i,3]==1){
			R=cbind(R,(thistotal[,"total"]-out$MeanPointsLocal2011))
			}
		else{
			R=cbind(R,(thistotal[,"total"]-(out$MeanPointsLocal2011+out$MeanPointsNBA2011)))
			}
		printt(R[1:3,])
		thisprice=(out$Points2011 + rowSums(R) * 0.5) * ScaleS + minpriceS + thisPriceWR$PriceWR
		
		# Add to the output:
		printt(round(thisprice,digits=1))
		out=cbind(out,round(thisprice,digits=1))
		printt(out[1:3,])
		}
	
	##### Output #####
	names(out)[10+seq_len(nrow(t))]=paste("Round",seq_len(nrow(t)),sep="")
	row.names(out)=NULL
	if(sort){
		out[order(out[,ncol(out)],decreasing=TRUE),]
		}
	else{
		out
		}
	##################################################
	##################################################
	}
