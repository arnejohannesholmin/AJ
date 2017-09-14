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
#' @rdname rank2bonus_old
#'
rank2bonus_old=function(x,A,level=4,bronze=TRUE){
	
	BonusCH=cbind(A=c(12,10,8,6,rep(5,4),rep(4,4),rep(3,4),ones(184)),B=c(3,2,1,0,zeros(196)))
	BonusBG6=cbind(A=c(10,8,6,4,rep(3,4),rep(2,4),rep(1,4),zeros(184)),B=c(3,2,1,0,zeros(196)))
	BonusWT=cbind(A=c(8,6,4,3,rep(2,4),rep(1,4),zeros(188)),B=c(3,2,1,0,zeros(196)))
	BonusNBA=cbind(A=c(6,4,3,2,rep(1,4),zeros(192)),B=c(2,1,0,0,zeros(196)))
	BonusLOCAL=cbind(A=c(1,0,0,zeros(197)),B=zeros(200))
	BonusOTHER=cbind(A=c(1,zeros(199)),B=zeros(200))
	bonusall=cbind(BonusCH,BonusBG6,BonusWT,BonusNBA,BonusLOCAL,BonusOTHER)
		
	#			CH	BG6	WT	NBA	LOC	OTH
	#	A1		12	10	8	6	3	1
	#	A2		10	8	6	4	2	0
	#	A3		8	6	4	3	1	0
	#	A4		6	4	3	2	0	0
	#	A5-8	5	3	2	1	0	0
	#	A9-12	4	2	1	0	0	0
	#	A13-16	3	1	0	0	0	0
	#	A17-	1	0	0	0	0	0
	#	
	#	B1		3	3	3	2	0	0
	#	B2		2	2	2	1	0	0
	#	B3		1	1	1	0	0	0
	#	B4		0	0	0	0	0	0
	#	B5-8	0	0	0	0	0	0

	if(!bronze && length(x)>3){
		x[3:4]=3
		}
	notplayed=duplicated(x)
	notplayed[c(diff(notplayed)>0,FALSE)]=TRUE
	rankB=x-A
	rankB[rankB<1]=NA
	x[x>A]=NA
	out=rowSums(cbind(bonusall[x,level*2-1],bonusall[rankB,level*2]),na.rm=TRUE)
	#if(level<5){
	#	out[notplayed]=out[notplayed]-0.5
	#	}
	out
	}
