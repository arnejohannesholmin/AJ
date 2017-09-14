#*********************************************
#*********************************************
#' ***DECRIPTION MISSING***
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD NAs ones zeros
#'
#' @export
#' @rdname get.points_fantasyNBA
#'
get.points_fantasyNBA<-function(dir,pow=c(3,3,3,3,1,1),w=c(32,32,31,28,10,6),t="all",n="all"){
	
	# - 't' is a two element vector specifying the time period.
	
	# dir="/Users/arnejohannesholmin/Documents/Diverse/Bordhockey/FantasyNBA/Tournaments"
	
	BonusCH=cbind(A=c(12,10,8,6,rep(5,4),rep(4,4),rep(3,4),ones(184)),B=c(3,2,1,0,zeros(196)))
	BonusBG6=cbind(A=c(10,8,6,4,rep(3,4),rep(2,4),rep(1,4),zeros(184)),B=c(3,2,1,0,zeros(196)))
	BonusWT=cbind(A=c(8,6,4,3,rep(2,4),rep(1,4),zeros(188)),B=c(3,2,1,0,zeros(196)))
	BonusNBA=cbind(A=c(6,4,3,2,rep(1,4),zeros(192)),B=c(2,1,0,0,zeros(196)))
	BonusLOCAL=cbind(A=c(1,zeros(199)),B=zeros(200))
	BonusOTHER=cbind(A=c(zeros(200)),B=zeros(200))
	bonusall=cbind(BonusCH,BonusBG6,BonusWT,BonusNBA,BonusLOCAL,BonusOTHER)
		
	#			CH	BG6	WT	NBA	LOC	OTH
	#	A1		12	10	8	6	1	0
	#	A2		10	8	6	4	0	0
	#	A3		8	6	4	3	0	0
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

	f=list.files(dir,full.names=TRUE)
	suppressWarnings(A<-as.numeric(substr(f, unlist(regexec("_A_",f))+3, nchar(f))))
	A[is.na(A)]=1000
	
	# Read the tournaments:
	tournamentnames=NAs(length(f))
	dates=NAs(length(f))
	names=vector("list",length(f))
	nations=vector("list",length(f))
	clubs=vector("list",length(f))
	ranks=vector("list",length(f))
	points=vector("list",length(f))
	type=vector("list",length(f))
	bronze=!logical(length(f))
	for(i in seq_along(f)){
		suppressWarnings(a<-readfromWR(f[i]))
		tournamentnames[i]=a$tournament
		dates[i]=a$dates
		names[[i]]=a$names
		names[[i]]=gsub("\"","",names[[i]])
		nations[[i]]=a$nations
		clubs[[i]]=a$clubs
		ranks[[i]]=a$ranks
		points[[i]]=a$points
		type[[i]]=a$type
		if(length(ranks[[i]])>3 && ranks[[i]][3] == ranks[[i]][4]){
			bronze[i]=FALSE
			}
		}
	# Order chronologically:
	tournamentnames=tournamentnames[order(dates)]
	names=names[order(dates)]
	nations=nations[order(dates)]
	clubs=clubs[order(dates)]
	ranks=ranks[order(dates)]
	points=points[order(dates)]
	type=type[order(dates)]
	level=getLevel(type)
	A=A[order(dates)]
	f=f[order(dates)]
	bronze=bronze[order(dates)]
	dates=dates[order(dates)]
	
	# Apply the daterange:
	if(t!="all" && length(t)>1){
		t=as.numeric(t)
		dates=as.numeric(dates)
		chosen= which(t[1]<=dates & dates<=t[2])
		if(n=="all"){
			n=length(chosen)
			}
		else{
			n=min(n,length(chosen))
			}
		# select the subset:
		tournamentnames=tournamentnames[chosen[seq_len(n)]]
		names=names[chosen[seq_len(n)]]
		nations=nations[chosen[seq_len(n)]]
		clubs=clubs[chosen[seq_len(n)]]
		ranks=ranks[chosen[seq_len(n)]]
		points=points[chosen[seq_len(n)]]
		type=type[chosen[seq_len(n)]]
		level=level[chosen[seq_len(n)]]
		A=A[chosen[seq_len(n)]]
		dates=dates[chosen[seq_len(n)]]
		}
		
	maxpoint=c(1,0.99,0.98,0.9,0.7,0.4)*1000
	
	score=ranks
	bonus=ranks
	totalscore=ranks
	
	for(i in seq_along(bonus)){
		score[[i]]=points2score(points[[i]],pow=pow[level[[i]]],w=w[level[[i]]],maxp=maxpoint[level[[i]]],bronze=bronze[i])
		if(tolower(type[i])=="norwegia"){
			bonus[[i]]=rank2bonus(ranks[[i]],A[i],3,bronze=bronze[i])
			}
		else{
			bonus[[i]]=rank2bonus(ranks[[i]],A[i],level[[i]],bronze=bronze[i])
			}
		totalscore[[i]]=score[[i]]+bonus[[i]]
		}
	
	
	
	nation=c("Austria","Belarus","Canada","Croatia","Czech Republic","Denmark","Estonia","Finland","Germany","Great Britain","Hungary","Latvia","Lithuania","Netherlands","Norway","Russia","Slovenia","Sweden","Switzerland","Ukraine","USA")
	NATION=c("AUS","BEL","CAN","CRO","CZE","DEN","EST","FIN","GER","GBR","HUN","LAT","LIT","NED","NOR","RUS","SLO","SWE","SUI","UKR","USA")
	nations=lapply(nations,function(x) if(any(x %in% nation)) NATION[match(x,nation)] else x)
		
	outlist=list(tournamentnames=tournamentnames,dates=dates,type=type,level=level,ranks=ranks,names=names,clubs=clubs,nations=nations,points=points,score=score,bonus=bonus,totalscore=totalscore)
		
	
	outlistT=vector("list",length(outlist[[1]]))
	namesdatT=vector("list",length(outlist[[1]]))
	
	usevar=c("dates","tournamentnames")
	for(i in 2:1){
		for(j in seq_along(outlist[[1]])){
			namesdatT[[j]]=cbind(namesdatT[[j]],outlist[[i]][[j]])
			}
		}
	for(i in 5:length(outlist)){
		for(j in seq_along(outlist[[1]])){
			outlistT[[j]]=cbind(outlistT[[j]],outlist[[i]][[j]])
			}
		}
	for(j in seq_along(outlist[[1]])){
		colnames(outlistT[[j]])=c("Rank","Player","Club","Nation","WRPoints","WRScore","Bonus","Score")
		}
	namesdatT=sapply(namesdatT,paste,collapse="_")
	names(outlistT)=namesdatT
	
	
	
	
	
	# Restructure to make a list of the scores for each player:
	allplayers=getallplayers()[,1]	
	totscore=vector("list",length(allplayers))
	totscoreNBA=vector("list",length(allplayers))
	totscoreLocal=vector("list",length(allplayers))
	for(i in seq_along(allplayers)){
		for(j in seq_along(outlist[[1]])){
			if(allplayers[i] %in% outlist$names[[j]]){
				totscore[[i]] = c(totscore[[i]],outlist$totalscore[[j]][allplayers[i]==outlist$names[[j]]])
				if(outlist$level[[j]]<5){
					totscoreNBA[[i]] = c(totscoreNBA[[i]],outlist$totalscore[[j]][allplayers[i]==outlist$names[[j]]])
					}
				else{
					totscoreLocal[[i]] = c(totscoreLocal[[i]],outlist$totalscore[[j]][allplayers[i]==outlist$names[[j]]])
					}
				}
			}
		}
	# Get the total score of the specified time period:
	total=sapply(totscore,sum)
	totalNBA=sapply(totscoreNBA,sum)
	totalLocal=sapply(totscoreLocal,sum)
	totalall=data.frame(Player=allplayers,totalNBA=totalNBA,totalLocal=totalLocal,total=total)
	totalall[,2:4]=round(totalall[,2:4],digits=1)
	
	
	# Return a list of three elements representing the data organized by variables, by tournaments, and transformed to total points pr player:
	list(variables=outlist,tournaments=outlistT,total=totalall,dates=dates)
	}
