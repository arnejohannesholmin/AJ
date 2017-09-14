#*********************************************
#*********************************************
#' ***DECRIPTION MISSING***
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD dim_all FALSEs NAs zeros
#'
#' @export
#' @rdname BL_Table
#'
BL_Table<-function(dir, dirRnk="~/Documents/Diverse/Bordhockey/FantasyNBA/Rankings_2014", series=list("Bergensl",c("Bergen C","KM Berge")), season=NULL, league="BL", hjemas=FALSE){
	
	
	months = c("JA","FB","MR","AP","MA","JN","JL","AG","SP","OK","NV","DS")
	b = basename(dir)
	fullSeason = nchar(b)>7
	date = as.numeric(substr(b,1,6))
	
	# New system from spring 2012 and on:
	p = c(100,85, 75,65,  60,55,51,47,  44,41,38,35, 32,30,28,26,  24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)
	pKM = p
	# Old system, where KM counts as special (60 points for first place before 2006, 40 after):
	pold = c(30,25,20,15,13,11:0,zeros(23))
	poldKM = c(40,35,30,25,20,15,13,11:0,zeros(21))
	poldestKM = c(60,55,50,45,40,38,36,34, 30,29,28,27,26,25,24,23, 20:1, zeros(4))
	
	
	# S	h/H	N.5	N1
	# H03	h,H	8	12	
	# V04	h,H	8	12	
	# H04	h,H	8	16	
	# V05	h,H	8	16	
	# H05	1	8	16	
	# V06	1	8	16	
	# H06	1	12	16	
	# V07	1	16	16	
	# H07	0.5	13	26	
	# V08	0.5	13	26	
	# H08	0.5	13	26	
	# V09	0.5	13	26	
	# H09	0.5	13	-	
	# V10	0.5	13	-	
	# H10	0.5	13	-	
	# V11	0.5	13	-	
	# H11	0.5	13	-	
	# V12	0.5	13	-	
	# H12	0.5	13	-	
	# V13	0.5	13	-	
	# H13	0.5	13	-	
	# V14	0.5	13	-	
	# 
	
	if(date<=201112){
		p = pold
		}
	
	
	if(date<=200506){
		pKM = poldestKM
		}
	else if(date<=201112){
		pKM = poldKM
		}
	

	if(date<=200606){
		nvalid = 8
		}
	else if(date<=200706){
		nvalid = 12
		}
	else{
		nvalid = 13
		}
	
	
	if(date<=200406){
		nvalid_fullSeason = 12
		}
	else if(date<=200706){
		nvalid_fullSeason = 16
		}
	else{
		nvalid_fullSeason = 23
		}
	if(fullSeason){
		nvalid = nvalid_fullSeason
		}
	
	
	# Create table for the Bergensliga:
	f = list.files(dir,full.names=TRUE)
	fr = list.files(dirRnk,full.names=TRUE)
	fr_dates = gsub("-", "", substr(fr, nchar(fr)-13, nchar(fr)-4))
	
	dates = NAs(length(f))
	names = vector("list",length(f))
	rank = vector("list",length(f))
	hjem = vector("list",length(f))
	clubs = vector("list",length(f))
	type = vector("list",length(f))
	for(i in seq_along(f)){
		# Read the tournament file:
		suppressWarnings(a<-readfromWR(f[i],nsign=8))
		dates[i] = a$dates
		names[[i]] = a$names
		clubs[[i]] = a$clubs
		type[[i]] = a$type
		
		# Read the last ranking:
		before = which(fr_dates < dates[i])
		lastRankFileBefore = which.max(fr_dates[before])
		suppressWarnings(aa<-read.table(fr[before[lastRankFileBefore]], skip=1, header=TRUE, sep="\t"))
		# Match the players with the ranking:
		rank[[i]] = rank(match(names[[i]], aa[,3]))
		hjem[[i]] = rank[[i]] - seq_along(rank[[i]])
	}
	# Order chronologically:
	names = names[order(dates)]
	clubs = clubs[order(dates)]
	type = type[order(dates)]
	rank = rank[order(dates)]
	hjem = hjem[order(dates)]
	dates = dates[order(dates)]
	
	
	fromlast = rev(!duplicated(rev(unlist(names))))
	players = unlist(names)[fromlast]
	playersclubs = unlist(clubs)[fromlast]
	playersclubs[nchar(playersclubs)<2] = NA
	
	points = NAs(length(players),length(f))
	GSB = NAs(length(players),length(f))
	
	rownames(points) = players
	rownames(GSB) = players
	for(i in seq_along(f)){
		GSB[names[[i]][1:3],i] = 1:3
		if(type[[i]]==series[[1]]){
			points[names[[i]],i] = p[seq_along(names[[i]])]
			}
		else if(type[[i]] %in% series[[2]]){
			points[names[[i]],i] = pKM[seq_along(names[[i]])]
			}
		if(hjemas){
			points[names[[i]],i] = hjem[[i]]
			}
		}
	
	GSB = apply(GSB,1,table)
	GSB = GSB[sapply(GSB,length)>0]
	
	#GSB = as.data.frame(GSB)
	deltakarar = colSums(!is.na(points))
	GSBout = zeros(length(GSB),3)
	for(i in seq_along(GSB)){
		GSBout[i,as.numeric(names(GSB[[i]]))] = GSB[[i]]
		}
	rownames(GSBout) = names(GSB)
	colnames(GSBout) = c("G","S","B")
	GSBout = GSBout[order(GSBout[,1],GSBout[,2],GSBout[,3], decreasing =TRUE),]
	GSBout = as.data.frame(GSBout)
	printt(GSBout)
		
	colnames(points) = dates
	
	
	##### Get processed data: #####
	valid = FALSEs(dim(points))
	for(i in seq_len(nrow(points))){
		present=!is.na(points[i,])
		if(sum(present)>nvalid){
			o = order(points[i,],decreasing=TRUE)
			valid[i,head(o,nvalid)] = TRUE
			}
		else{
			valid[i,] = present
			}
		}
	
	validpoints = points
	validpoints[!valid] = NA
	validpoints0 = points
	validpoints0[!valid] = 0
	sumpoints = apply(validpoints0,1,cumsum)
	if(length(dim_all(sumpoints))==1){
		dim(sumpoints) = c(1,length(sumpoints))
		}
	expsumpoints = apply(exp(1)^validpoints0,1,cumsum)
	if(length(dim_all(expsumpoints))==1){
		dim(expsumpoints) = c(1,length(expsumpoints))
		}
	# Inset NA at 0 in 'sumpoints':
	#sumpoints[sumpoints==0] = NA
	expsumpoints[expsumpoints==0] = NA
	totalcounts = rowSums(!is.na(points))
	counts = totalcounts
	counts[counts>nvalid] = nvalid
	Max = apply(points,1,max,na.rm=TRUE)
	average = round(rowMeans(validpoints,na.rm=TRUE),digits=2)
	totalsumpoints = rowSums(points,na.rm=TRUE)
	totalaverage = round(rowMeans(points,na.rm=TRUE),digits=2)
	#write.table(points,file=paste(league,b,".dat",sep=""),row.names=TRUE,col.names=TRUE, quote=FALSE,sep = "\t")
	ordersums = order(sumpoints[nrow(sumpoints),],expsumpoints[nrow(sumpoints),],decreasing=TRUE,na.last=NA)
	#ordersums = orderplayers(sumpoints,expsumpoints)
	#####
	
	##### Old order: #####
	if(length(f)>1){
		old_points = points[,-ncol(points),drop=FALSE]
		old_valid = FALSEs(dim(old_points))
		for(i in seq_len(nrow(old_points))){
			old_present=!is.na(old_points[i,])
			if(sum(old_present)>nvalid){
				old_o = order(old_points[i,],decreasing=TRUE)
				old_valid[i,head(old_o,nvalid)] = TRUE
				}
			else{
				old_valid[i,] = old_present
				}
			}
		
		old_hasnotpoints = rowSums(is.na(old_points))==ncol(old_points)
		old_validpoints0 = old_points
		old_validpoints0[!old_valid] = 0
		old_sumpoints = apply(old_validpoints0,1,cumsum)
		if(length(dim_all(old_sumpoints))==1){
			dim(old_sumpoints) = c(1,length(old_sumpoints))
			}
		old_expsumpoints = apply(exp(1)^old_validpoints0,1,cumsum)
		if(length(dim_all(old_expsumpoints))==1){
			dim(old_expsumpoints) = c(1,length(old_expsumpoints))
			}
		old_ordersums = order(old_sumpoints[nrow(old_sumpoints),], old_expsumpoints[nrow(old_sumpoints),], decreasing=TRUE, na.last=NA)
		# Insert NAs for new players:
		if(sum(old_hasnotpoints)>0){
			old_ordersums[length(old_ordersums)+seq(1-sum(old_hasnotpoints),0)] = NA
			}
		}
	else{
		old_ordersums = rep(NA,nrow(points))
		}
	#####
	
	
	# Enclose in paraenthesis to identify not counted results (in the nvalid):
	points[!valid & !is.na(points)] = paste("(",points[!valid & !is.na(points)],")",sep="")
	
	
	
	points = cbind(points,Sum=sumpoints[nrow(sumpoints),],Count=counts,Max,Avg=average,SumT=totalsumpoints,CountT=totalcounts,AvgT=totalaverage)
	points = points[ordersums,]
	Prev = match(seq_along(ordersums),old_ordersums)[ordersums]
	points = cbind(Nr=seq_len(nrow(points)),Prev=Prev,Player=players[ordersums],Club=playersclubs[ordersums],points)
	points = rbind(points,c(rep(NA,2),"Participants",NA,deltakarar,c(sum(deltakarar),length(deltakarar),max(deltakarar),round(mean(deltakarar),digits=1),NA,NA,NA)))
	
	points[is.na(points)] = ""
	
	# Replace dots by commas:
	points = gsub(".",",",points,fixed=TRUE)
	
	# Add dates:
	points = rbind(colnames(points),colnames(points),points)
	suppressWarnings(isnum<-!is.na(as.numeric(points[1,])))
	if(length(season)==0){
		season = paste(c("Vaar","Haust")[(as.numeric(substr(points[1,isnum][1],5,6))>6) + 1],substr(b,1,4))
		}
	points[2,isnum] = order(points[2,isnum])
	points[1,!isnum] = ""
	points[1,isnum] = paste(substring(points[1,isnum],7),months[as.numeric(substr(points[1,isnum],5,6))],sep=".")
	points[1,3] = season
	points[1,4] = "Dato:"
	
	
	
	setwd(dirname(dir))
	write.table(x=points,file=paste0(league, b, if(hjemas) "Hjemas" else "", ".xls",sep=""),row.names=FALSE,col.names=FALSE, quote=FALSE,sep = "\t")
	}		
