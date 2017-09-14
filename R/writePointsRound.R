#*********************************************
#*********************************************
#' ***DECRIPTION MISSING***
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom gdata read.xls
#' @importFrom TSD chars NAs ones zeros
#'
#' @export
#' @rdname writePointsRound
#'
writePointsRound<-function(round=1,volume=1,pow=c(3,3,3,3,1,1),w=c(32,32,31,28,10,6),subst=TRUE,write=c("StatsPlayersRound","PointsRound","TableRound","form")){
	
	# In "StatsPlayersRound" the values are updated by the latest WR and collected points. In "PointsRound" and "TableRound" and "form" the values at the start of the round is used, unless volume=="all".


	charvector=c("","*","**","***","****","*****","******","*******","********","*********","**********","***********","************","*************","**************","***************","****************","*****************","******************","*******************","********************","*********************","**********************","***********************","************************","*************************")
	nrounds=min(13,round)
	nparticipants=100
	pos=c("Keeper","Left defender","Rigth defender","Left wing","Center","Right wing","Substitute 1","Substitute 2")
	maxprice=50
	
	reorder=function(x){
		order=seq_along(x)
		arezeros=which(x[1:6]==0)
		subst=which(x[7:8]>0)
		
		if(length(arezeros)>0 && length(subst)>0){
			if(length(arezeros)<length(subst)){
				out=order[arezeros[seq_along(arezeros)]]
				order[arezeros[seq_along(arezeros)]]=subst[seq_along(arezeros)]+6
				order[subst[seq_along(arezeros)]+6]=out
				}
			else{
				out=order[arezeros[seq_along(subst)]]
				order[arezeros[seq_along(subst)]]=subst+6
				order[subst+6]=out
				}
			}
		order
		}
	
	# Get all players:
	allplayers=getallplayers()
	nplayers=nrow(allplayers)
	allplayers[,1]=substr(allplayers[,1],1,20)
	allplayerstables=vector("list",nrow(allplayers))
	
	# Read deadlines:
	t=as.matrix(read.xls("/Users/arnejohannesholmin/Documents/Diverse/Bordhockey/FantasyNBA/Deadlines/Deadlines2012.xls")[,2:3])
	
	dir="/Users/arnejohannesholmin/Documents/Diverse/Bordhockey/FantasyNBA/Tournaments"
	
	teamdir=list.files("/Users/arnejohannesholmin/Documents/Diverse/Bordhockey/FantasyNBA/FantasyNBA testsesong vår 2012/",full.names=TRUE)
	teamdirnames=basename(teamdir)
	valid=substr(teamdirnames,1,5)=="Runde"
	teamdir=teamdir[valid]
	teamdirnames=teamdirnames[valid]
	r=as.numeric(substr(teamdirnames,7,8))
	
	
	teamnames=NULL
	oldteamnames=NULL
	teams=NAs(8,nrounds,nparticipants)
	points=NAs(8,nrounds,nparticipants)
	values=NAs(8,nrounds,nparticipants)
	ntransfers=NAs(nrounds,nparticipants)
	bank=NAs(nrounds,nparticipants)
	oldprice=NULL
	diffprice=NULL
	# 'oldteams' is useful when extracting which players are sold and bought, and how many times:
	oldteams=NULL
	dreamteamRounds=zeros(nplayers,nrounds)
	dreamteamTotal=zeros(nplayers,nrounds)
	pTotalforDreamteamTotal=NULL
	
	gameweekjoined=NULL
	
	for(j in seq_len(nrounds)){
		# Get player points:
		p=get.points_fantasyNBA(dir,pow=pow,w=w,t=t[j,],n=if(j==nrounds) volume else "all")
		
		# Get player prices:
		if(substr(t[j,1],4,8)=="0101"){
			t[j,1]=paste(as.numeric(substr(t[j,1],1,4))-1,"1231",sep="")
			}
		if(substr(t[j,1],7,8)=="01" && substr(t[j,1],5,6)%in%c("01","02","04","06","08","09","11")){
			t[j,1]=paste(as.numeric(substr(t[j,1],1,6))-1,"31",sep="")
			}
		else if(substr(t[j,1],7,8)=="01" && substr(t[j,1],5,6)%in%c("05","07","10","12")){
			t[j,1]=paste(as.numeric(substr(t[j,1],1,6))-1,"30",sep="")
			}
		else if(substr(t[j,1],7,8)=="01" && substr(t[j,1],5,6)=="03"){
			t[j,1]=paste(as.numeric(substr(t[j,1],1,6))-1,"28",sep="")
			}
		else{
			t[j,1]=t[j,1]-1
			}
		price=getprice_testseason(t[j,1])
		price[,1]=substr(price[,1],1,20)
		p$total$Player=substr(p$total$Player,1,20)
		p$total=cbind(p$total,Dreamteam=0)
		pTotalforDreamteamTotal=cbind(pTotalforDreamteamTotal,p$total$total-price[,ncol(price)]/1000000)
		
		if(!(j==nrounds && volume==0)  && length(p$dates)>0){
			p$total$Dreamteam[order(p$total$total-price[,ncol(price)]/1000000,decreasing=TRUE)[1:6]]=1
			p$total$Dreamteam[p$total$total==0]=0
			dreamteamRounds[,j]=p$total$Dreamteam
			
			dreamteamTotal[order(rowSums(pTotalforDreamteamTotal),decreasing=TRUE)[1:6],j]=1
			dreamteamTotal[rowSums(pTotalforDreamteamTotal)==0]=0
			}
		dreamteamRounds[is.na(dreamteamRounds)]=0
															
		
		# Read the teams:
		thisplayers=readTeams(teamdir[r==j])
		thisteamnames=colnames(thisplayers)
		newteamnames=setdiff(thisteamnames,teamnames)
		
		# Add the new team names to 'teamnames':
		teamnames=c(teamnames,setdiff(thisteamnames,teamnames))
		gameweekjoined=c(gameweekjoined,rep(j,length(newteamnames)))
		teamseq=seq_along(teamnames)
		# Add the new teams to 'teams':
		if(j>1){
			teams[,j,]=teams[,j-1,]
			}
		teams[,j,match(thisteamnames,teamnames)]=thisplayers
		
		
		points[,j,teamseq]=p$total$total[match(teams[,j,teamseq],p$total$Player)]
		points[is.na(points)]=0
		thisprice=price[match(teams[,j,teamseq],price[,1]),ncol(price)]
		thisprice[is.na(thisprice)]=2
		values[,j,teamseq]=thisprice
		
		if(j==nrounds && volume=="all"){
			lastprice=getprice_testseason(t[j,2])
			lastprice[,1]=substr(lastprice[,1],1,20)
			thisprice=lastprice[match(teams[,j,teamseq],lastprice[,1]),ncol(lastprice)]
			thisprice[is.na(thisprice)]=2
			currentvalues=array(thisprice,dim=c(8,length(teamseq)))
			}
		else{
			currentvalues=values[,j,teamseq]
			}
		
		# Get the price changes:
		if(j==1){
			diffprice=data.frame(zeros(nrow(price)))
			}
		else{
			diffprice=price[ncol(price)]-oldprice[ncol(oldprice)]
			}
		colnames(diffprice)="Change"
		
		
		bought=NULL
		sold=NULL
		selected=round(table(teams[,j,])/length(teamnames)*100,digits=1)
		for(pl in seq_along(oldteamnames)){
			bought=c(bought,setdiff(teams[,j,pl],oldteams[,pl]))
			sold=c(sold,setdiff(oldteams[,pl],teams[,j,pl]))
			}
		if(j==1){
			bought=table(teams[,j,])
			}
		else{
			bought=table(bought)
			}
		sold=table(sold)
		
		
		for(pl in seq_along(allplayerstables)){
			WR_pl=price$PointsWR0[price[,1]==allplayers[pl]]
			sold_pl=sold[allplayers[pl]]
			bought_pl=bought[allplayers[pl]]
			selected_pl=selected[allplayers[pl]]
			points3_pl=p$total[p$total[,1]==allplayers[pl],2:4]
			total_pl=0
			price_pl=price[price[,1]==allplayers[pl],ncol(price)]
			pricechange_pl=diffprice[price[,1]==allplayers[pl],1]
			thisdreamteamRound=dreamteamRounds[price[,1]==allplayers[pl],j]
			thisdreamteamTotal=dreamteamTotal[price[,1]==allplayers[pl],j]
			
			thisline=unlist(c(WR=WR_pl, Sold=sold_pl, Bought=bought_pl, Selected=selected_pl, Touraments=points3_pl[1], Local=points3_pl[2], Points=points3_pl[3], Total=total_pl, Price=price_pl, Change=pricechange_pl, DreamteamRound=thisdreamteamRound, DreamteamTotal=thisdreamteamTotal))
			thisline[is.na(thisline)]=0
			allplayerstables[[pl]]=rbind(allplayerstables[[pl]],thisline)
			}
		
		# Calculate the bank and reset the values to the current values:
		if(length(newteamnames)>0){
			bank[j,match(newteamnames,teamnames)]=maxprice-colSums(values[,j,match(newteamnames,teamnames),drop=FALSE],na.rm=TRUE)
			ntransfers[j,match(newteamnames,teamnames)]=0
			}
		if(j>1){
			values_oldteam=array(unlist(price[match(oldteams,price[,1]),ncol(price),drop=FALSE]),dim=dim(oldteams))
			values_oldteam[is.na(values_oldteam)]=2
			bank[j,match(oldteamnames,teamnames)]=bank[j-1,match(oldteamnames,teamnames)]-colSums(values[,j,match(oldteamnames,teamnames)],na.rm=TRUE)+colSums(values_oldteam,na.rm=TRUE)
			for(p in match(oldteamnames,teamnames)){
				ntransfers[j,p]=length(setdiff(teams[,j,p],teams[,j-1,p]))
				}
			}
		
		oldprice=price
		
		oldteams=teams[,j,teamseq]
		
		oldteamnames=teamnames
		}
	
			
	lastprice=getprice_testseason(t[j,2])
	# Define colnames to the tables for each player:
	for(pl in seq_along(allplayerstables)){
		allplayerstables[[pl]]=as.data.frame(allplayerstables[[pl]])
		colnames(allplayerstables[[pl]])=c("WR","Sold","Bought","Selected","Tournaments","Local","Points","Total","Price","Change","DreamteamRound","DreamteamTotal")
		row.names(allplayerstables[[pl]])=paste("Round",seq_len(nrounds),sep="")
		allplayerstables[[pl]]$DreamteamRound=charvector[dreamteamRounds[pl,]+1]
		allplayerstables[[pl]]$DreamteamTotal=charvector[dreamteamTotal[pl,]+1]
		
		# Add up the points to get the cummulated points:
		allplayerstables[[pl]]$Total=cumsum(allplayerstables[[pl]]$Points)
		# Move the price change to the week they resulted from:
		allplayerstables[[pl]]$Change[-nrounds]=allplayerstables[[pl]]$Change[-1]
		allplayerstables[[pl]]$Change[nrounds] = lastprice[substr(allplayers[pl,1],1,20)==substr(lastprice[,1],1,20),ncol(lastprice)] - allplayerstables[[pl]]$Price[nrounds]
		}
	names(allplayerstables)=allplayers[,1]
		
	
	if("StatsPlayersRound" %in% write){
		file=paste("/Users/arnejohannesholmin/Documents/Diverse/Bordhockey/FantasyNBA/Handouts new/StatsPlayersRound_",j,"_",volume,".xls",sep="")
		ordertotalpoints=order(sapply(allplayerstables,function(x) x[nrow(x),8]),decreasing=TRUE)
		for(p in seq_along(ordertotalpoints)){
			write.table(array(c(paste(p,names(allplayerstables[ordertotalpoints[p]]),sep=": "),character(9)),dim=c(1,9)),file,append=p>1,sep="\t",row.names=FALSE,col.names=FALSE)
			suppressWarnings(write.table(allplayerstables[[ordertotalpoints[p]]],file,append=TRUE,sep="\t",row.names=FALSE,dec=","))
			if(p<length(allplayerstables)){
				suppressWarnings(write.table(chars(2,10),file,append=TRUE,sep="\t",row.names=FALSE,col.names=FALSE))
				}
			}
		}		
		
		
		
		
	
	
		
	# Determine which teams are participating and define minus points due to transfers:
	validteams=which(!is.na(teams[1,nrounds,]))
	ntransfers[is.na(ntransfers)]=0
	
	freetransfers=ones(nrounds+1,nparticipants)
	freetransfers[1,]=0
	for(j in seq_len(nrounds+1)){
		if(j>2){
			# If the team has not played a full round, 
			get2= (ntransfers[j-1,]==0 | (freetransfers[j-1,]==2 & ntransfers[j-1,]<2)) & c(j>gameweekjoined+1,logical(nparticipants-length(gameweekjoined)))
			freetransfers[j,get2]=2
			}
		}
	minuspoints=8*(ntransfers-freetransfers[seq_len(nrounds),])
	minuspoints[minuspoints<0]=0
	#minuspoints=minuspoints[,validteams,drop=FALSE]
	
	
	allrounds=vector("list",nrounds)
	for(j in seq_len(nrounds)){
		thisPosition=c(pos,paste("Round",j),"Total","Transfers","TransfersTotal","Subtracted","SubtractedTotal","Value","Bank")
		validteams=which(!is.na(teams[1,j,]))
		allrounds[[j]]=vector("list",length(validteams))
		names(allrounds[[j]])=teamnames[validteams]
			
		file=paste("/Users/arnejohannesholmin/Documents/Diverse/Bordhockey/FantasyNBA/Handouts new/PointsRound_",j,"_",volume,".xls",sep="")
		for(p in order(teamnames[validteams])){
			if(subst || j<nrounds){
				neworder=reorder(points[1:8,j,p])
				teams[1:8,j,p]=teams[neworder,j,p]
				points[1:8,j,p]=points[neworder,j,p]
				values[1:8,j,p]=values[neworder,j,p]
				}		
			thisp=validteams[p]
			thisteam=c(teams[,j,thisp],character(8))
			thispoints=c(p1t8=points[,j,thisp], p9=sum(points[1:6,j,thisp],na.rm=TRUE), p10=sum(points[1:6,seq_len(j),thisp],na.rm=TRUE)-sum(minuspoints[seq_len(j),thisp],na.rm=TRUE), p11=ntransfers[j,thisp], p12=sum(ntransfers[seq_len(j),thisp],na.rm=TRUE), p13=minuspoints[j,thisp], p14=sum(minuspoints[seq_len(j),thisp],na.rm=TRUE), p15=sum(values[,j,thisp],na.rm=TRUE), p16=bank[j,thisp])
			thisprice=c(gsub(".",",",c(price[match(teams[,j,thisp],allplayers),ncol(price)]),fixed=TRUE),character(8))
			thisprice[is.na(thisprice)]=2
			thisdreamteamRound=c(charvector[dreamteamRounds[match(teams[,j,thisp],allplayers),j]+1],character(8))
			thisdreamteamTotal=c(charvector[dreamteamTotal[match(teams[,j,thisp],allplayers),j]+1],character(8))
			thisdreamteamRound[is.na(thisdreamteamRound)]=""
			thisdreamteamTotal[is.na(thisdreamteamTotal)]=""
			allrounds[[j]][[p]]=data.frame(Position=thisPosition,Player=thisteam,Price=thisprice,Points=thispoints,DreamteamRound=thisdreamteamRound,DreamteamTotal=thisdreamteamTotal)
			if(j==nrounds && "PointsRound" %in% write){
				write.table(array(c(names(allrounds[[j]][p]),character(5)),dim=c(1,5)),file,append=p>1,sep="\t",row.names=FALSE,col.names=FALSE)
				suppressWarnings(write.table(allrounds[[j]][[p]],file,append=TRUE,sep="\t",row.names=FALSE,dec=","))
				if(subst){
					for(s in which(neworder[1:6]!=1:6)){
						suppressWarnings(write.table(array(c(paste("Substituted: ",teams[neworder[s],j,p]," out, ",teams[s,j,p]," in",sep=""),"",""),dim=c(1,3)),file,append=TRUE,sep="\t",row.names=FALSE,col.names=FALSE))
						}
					}
				if(p<length(allrounds[[j]])){
					suppressWarnings(write.table(chars(2,6),file,append=TRUE,sep="\t",row.names=FALSE,col.names=FALSE))
					}
				}
			}
		}
	
	Team=teamnames
	RoundBrutto = t(apply(points[1:6,,validteams,drop=FALSE],2:3,sum,na.rm=TRUE))
	RoundNetto = t(apply(points[1:6,,validteams,drop=FALSE],2:3,sum,na.rm=TRUE))-t(minuspoints[,validteams,drop=FALSE])
	RoundTransfers = ntransfers[,validteams,drop=FALSE]
	RoundSubtracted = minuspoints[,validteams,drop=FALSE]	
	TotalBrutto=apply(points[1:6,seq_len(nrounds),validteams,drop=FALSE],3,sum,na.rm=TRUE)
	TotalSubtracted = colSums(minuspoints[,validteams,drop=FALSE],na.rm=TRUE)	
	TotalNetto=apply(points[1:6,seq_len(nrounds),validteams,drop=FALSE],3,sum,na.rm=TRUE)-TotalSubtracted
	TotalTransfers=colSums(ntransfers[,validteams,drop=FALSE],na.rm=TRUE)
	CurrentValue=colSums(currentvalues)
	#Value=t(apply(values[,nrounds,validteams,drop=FALSE],2:3,sum,na.rm=TRUE))
	Bank=bank[nrounds,validteams]
	TotalValue=CurrentValue+Bank
	
	if(length(RoundBrutto)>0){
		teamtable=data.frame(Team=Team, array(rbind(RoundBrutto,RoundNetto,t(RoundTransfers),t(RoundSubtracted)),dim=c(nrow(RoundBrutto),4*ncol(RoundBrutto))), TB=TotalBrutto, TN=TotalNetto, TT=TotalTransfers, TS=TotalSubtracted, FT=freetransfers[nrounds,validteams], CV=CurrentValue, B=Bank, TV=TotalValue)
		colnames(teamtable)=c("Team",outer(c("RB","RN","RT","RS"),seq_len(nrounds),paste,sep=""),"TB","TN","TT","TS","FT","CV","B","TV")
		teamtable=teamtable[order(teamtable$TN,decreasing=TRUE),]
		teamtable=cbind(Rank=floor(nrow(teamtable)+1-rank(teamtable$TN,ties.method="max")),teamtable)
		
		if("TableRound" %in% write){
			file=paste("/Users/arnejohannesholmin/Documents/Diverse/Bordhockey/FantasyNBA/Handouts new/TableRound_",j,"_",volume,".xls",sep="")
			write.table(teamtable,file,sep="\t",row.names=FALSE,col.names=TRUE,dec=",")
			}
		
		write.table(rbind("",cbind("",c("RB: RoundBrutto","RN: RoundNetto","RT: RoundTransfers","RS: RoundSubtracted","TB: TotalBrutto","TN: TotalNetto","TT: TotalTransfers","TS: TotalSubtracted","FT: FreeTransfers","CV=CurrentValue","B: Bank","TV: TotalValue"))),file,sep="\t",row.names=FALSE,col.names=FALSE,append=TRUE)
			
		}
	
	

	##### Finally write the gameweek form: #####
	# Excelark med informasjon om spelarane og høve til å velja lag:
	#	Rank	Player	Club	Nation	Sold	Bought	Selected	PriceChange	PointsWR	Best	Score2011	Scorev2012	ScoreRound	PriceT	*Lag*
	
	if("form" %in% write){
		formdir=paste("/Users/arnejohannesholmin/Documents/Diverse/Bordhockey/FantasyNBA/Handouts new/Forms round",nrounds,sep="")
		dir.create(formdir)
		setwd(formdir)
		}
		
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
	# Read prices:
	thisPriceWR=getpriceWR(date=t[nrounds,2],par=c(ScaleWR,minpriceWR))
		
	validteams=which(!is.na(teams[1,nrounds-1,]))
	for(p in seq_along(validteams)){
	
		Player=allplayers[,1]
		Club=allplayers[,2]
		Nation=allplayers[,3]
		transferweek=nrow(allplayerstables[[1]]) - as.numeric(j>1)
		Sold=sapply(allplayerstables,function(x) x[transferweek,2])
		Bought=sapply(allplayerstables,function(x) x[transferweek,3])
		Selected=sapply(allplayerstables,function(x) x[transferweek,4])
		Change=round(diffprice[match(allplayers[,1],price[,1]),1],digits=1)
		PointsWR=thisPriceWR$PointsWR
		Best=thisPriceWR$Best
		Score2011=2*price$Points2011[match(allplayers[,1],price[,1])]
		Scorev2012=sapply(allplayerstables,function(x) sum(x[seq_len(nrow(x)),8]))
		ScoreRound=sapply(allplayerstables,function(x) x[nrow(x),8])
		Price=price[match(allplayers[,1],price[,1]),ncol(price)]
		Position=c("Keeper","Left defender","Rigth defender","Left wing","Center","Right wing","Substitute 1","Substitute 2",character(length(Price)-8))
		Team=c(teams[,j,validteams[p]],"Total","Old Team Value","In the bank","Rest","","Free transfers",character(length(Price)-14))
		thisbank=max(bank[nrounds,validteams[p]],0)
		Value=c(character(9),sum(values[,nrounds,validteams[p]]),thisbank,"","",freetransfers[nrounds,validteams[p]],character(length(Price)-14))
		
		gameweekform=data.frame(Player=Player, Club=Club, Nation=Nation, Sold=Sold, Bought=Bought, Selected=Selected, Change=Change,PointsWR=PointsWR, Best=Best, Score2011=Score2011, Scorev2012=Scorev2012, ScoreRound=ScoreRound, Price=Price)
		gameweekform=cbind(Rank=floor(nrow(gameweekform)+1-rank(gameweekform$Price)),gameweekform)
		gameweekform=gameweekform[order(gameweekform$Price,decreasing=TRUE),]
		
		gameweekform=cbind(gameweekform, data.frame(Position=Position, Team=Team, Value=gsub(".",",",Value,fixed=TRUE)))
		
		if("form" %in% write){
			thisteamname=teamnames[p]
			thisteamname=strsplit(thisteamname," - ")[[1]]
			
			write.table(x=gameweekform,paste(thisteamname[1],"@",thisteamname[2],"@FantasyNBA runde ",nrounds,".xls",sep=""),row.names = FALSE,dec = ",",sep="\t")
			}
		}
	
	
	
	# Return various information:
	if(length(RoundBrutto)>0){
		list(allrounds=allrounds,teamtable=teamtable,allplayerstables=allplayerstables, gameweekform=gameweekform)
		}
	else{
		list(allrounds=allrounds,allplayerstables=allplayerstables, gameweekform=gameweekform)
		}
	}


