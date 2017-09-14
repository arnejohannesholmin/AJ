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
#' @rdname tournamentPointsBest
#'
tournamentPointsBest=function(Best,level=5,pow=c(3,3,3,3,1,1),w=c(32,32,31,28,10,6),bronze=TRUE,A=1000,players=NULL){
	
	maxpoint=c(1,0.99,0.98,0.9,0.7,0.4)*1000
	
	BonusCH=cbind(A=c(12,10,8,6,rep(5,4),rep(4,4),rep(3,4),ones(184)),B=c(3,2,1,0,zeros(196)))
	BonusBG6=cbind(A=c(10,8,6,4,rep(3,4),rep(2,4),rep(1,4),zeros(184)),B=c(3,2,1,0,zeros(196)))
	BonusWT=cbind(A=c(8,6,4,3,rep(2,4),rep(1,4),zeros(188)),B=c(3,2,1,0,zeros(196)))
	BonusNBA=cbind(A=c(6,4,3,2,rep(1,4),zeros(192)),B=c(2,1,0,0,zeros(196)))
	BonusLOCAL=cbind(A=c(1,zeros(199)),B=zeros(200))
	BonusOTHER=cbind(A=c(zeros(200)),B=zeros(200))
	bonusall=cbind(BonusCH,BonusBG6,BonusWT,BonusNBA,BonusLOCAL,BonusOTHER)
	
	o=order(Best,decreasing=TRUE)
	printt(Best)
	printt(o)
	if(length(players)){
		players=players[o]
		}
	Best=Best[o]
	p=rank2wrPoints(Best,level=level)
	
	if(!bronze){
		p[3:4]=floor(mean(p[3:4]))
		}
	
	PointsWR=points2score(p,pow[level],w[level],maxpoint[level],bronze=bronze)
	PointsBonus=rank2bonus(seq_along(p),A,level=level,bronze=bronze)
	Points=PointsWR+PointsBonus
	Rank=seq_along(p)
	if(!bronze){
		Rank[4]=3
		}
	cbind(players,Rank=Rank,Best=Best,WR=p,PointsWR,PointsBonus,Points)
	}
