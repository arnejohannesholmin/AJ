#*********************************************
#*********************************************
#' ***DECRIPTION MISSING***
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD ones zeros
#'
#' @export
#' @rdname tournamentPoints
#'
tournamentPoints=function(players,tournament,date,series,pow=c(3,3,3,3,1,1),w=c(32,32,31,28,10,6),bronze=TRUE,A=1000){
	
	maxpoint=c(1,0.99,0.98,0.9,0.7,0.4)*1000
	
	BonusCH=cbind(A=c(12,10,8,6,rep(5,4),rep(4,4),rep(3,4),ones(184)),B=c(3,2,1,0,zeros(196)))
	BonusBG6=cbind(A=c(10,8,6,4,rep(3,4),rep(2,4),rep(1,4),zeros(184)),B=c(3,2,1,0,zeros(196)))
	BonusWT=cbind(A=c(8,6,4,3,rep(2,4),rep(1,4),zeros(188)),B=c(3,2,1,0,zeros(196)))
	BonusNBA=cbind(A=c(6,4,3,2,rep(1,4),zeros(192)),B=c(2,1,0,0,zeros(196)))
	BonusLOCAL=cbind(A=c(1,zeros(199)),B=zeros(200))
	BonusOTHER=cbind(A=c(zeros(200)),B=zeros(200))
	bonusall=cbind(BonusCH,BonusBG6,BonusWT,BonusNBA,BonusLOCAL,BonusOTHER)
	
	
	dir=file.path(Sys.getenv("HOME"),"Documents/Diverse/Bordhockey/FantasyNBA/Tournaments")
	
	
	thisdate=rev(strsplit(as.character(date),".",fixed=TRUE)[[1]])
	thisdate[nchar(thisdate)==1]=paste("0",thisdate[nchar(thisdate)==1],sep="")
	thisdate=paste(thisdate,collapse="")
	
	out=data.frame(a=players,b=rep("-",length=length(players)),c=rep("-",length=length(players)),d=rep("-",length=length(players)),e=ones(length(players)))
	out[,1]=as.character(out[,1])
	out[,2]=as.character(out[,2])
	out[,3]=as.character(out[,3])
	WR=getWR(date=thisdate)
	WR=WR[WR$Player %in% players,]
	out[match(WR$Player,players),2]=as.character(WR$Club)
	out[match(WR$Player,players),3]=as.character(WR$Nation)
	out[match(WR$Player,players),5]=WR$Best
	
	level=min(getLevel(tournament),getLevel(series))
	printt(level)
	
	if(is.numeric(players)){
		p=sort(players,decreasing=TRUE)
		cat("Level",level)
		Rank=seq_along(p)
		if(!bronze){
			Rank[4]=3
			}
		cbind(Rank=Rank,Best=p,WR=p,Points=points2score(p,pow[level],w[level],maxpoint[level],bronze=bronze)+rank2bonus(length(p),A,level=level,bronze=bronze))
		}
	else{
		p=rank2wrPoints(out[,5],level=level,bronze=bronze)
		cat("Level",level)
		PointsWR=points2score(p,pow[level],w[level],maxpoint[level],bronze=bronze)
		PointsBonus=rank2bonus(length(p),A,level=level,bronze=bronze)
		Points=PointsWR+PointsBonus
		Rank=seq_along(p)
		if(!bronze){
			Rank[4]=3
			}
		cbind(Rank=Rank,Player=players,Best=out[,5],WR=p,PointsWR,PointsBonus,Points)
		}
	}
