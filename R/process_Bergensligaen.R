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
#' @rdname process_Bergensligaen
#'
process_Bergensligaen=function(seasons=c("H2011","V2012"),type=c("points","GSB"),p=c("Holmin","Old","FIS","Biathlon"),v=13){
	
	# library("dataframes2xls")

	if(!is.numeric(p)){
		if(tolower(substr(p[1],1,1))=="h"){
			p=c(100,85, 75,65,  60,55,51,47,  44,41,38,35, 32,30,28,26,  24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)
			pKM=p
			}
		else if(tolower(substr(p[1],1,1))=="o"){
			p=c(30,25,20,15,13,11:0,zeros(20))
			pKM=c(40,35,30,25,20,15,13,11:0,zeros(20))
			}
		else if(tolower(substr(p[1],1,1))=="f"){
			#p=c(30,25,20,15,13,11:0,zeros(20))
			#pKM=c(40,35,30,25,20,15,13,11:0,zeros(20))
			}
		else if(tolower(substr(p[1],1,1))=="b"){
			#p=c(30,25,20,15,13,11:0,zeros(20))
			#pKM=c(40,35,30,25,20,15,13,11:0,zeros(20))
			}
		}
	
	v=rep(v,length(seasons))
	
	pointslist=vector("list",length(seasons))
	validlist=vector("list",length(seasons))
	players=vector("list",length(seasons))
	playersclubs=vector("list",length(seasons))
	for(s in seq_along(seasons)){
		dir=file.path("/Users/arnejohannesholmin/Documents/Diverse/Bordhockey/BSI Bordhockey/Bergensligaen/Ligastillingar",seasons[s])
		
		this=process_Bergensliga1(seasons[s],p,pKM)
		
		pointslist[[s]]=this$points
		players[[s]]=this$players
		playersclubs[[s]]=this$playersclubs
		
		validlist[[s]]=FALSEs(dim(pointslist[[s]]))
		for(i in seq_len(nrow(pointslist[[s]])-1)){
			o=order(pointslist[[s]][i,],decreasing=TRUE)
			validlist[[s]][i,head(o,v[s])]=TRUE
			}
		}
		
	revplayers=rev(players)
	uniquerevplayers=unlist(revplayers)
	rev(!duplicated(uniqueplayers))
	
	
	
	players=lapply(pointslist,rownames)
	
	uniqueplayers=unlist(players)
	
	printt(players)
	printt(playersclubs)
	
	uniqueplayersclubs=unlist(playersclubs)
	uniqueplayersclubs=uniqueplayersclubs[!duplicated(uniqueplayers)]
	
	uniqueplayers=unique(uniqueplayers)
	
	
	rounds=sapply(dim_all(pointslist),tail,1)
	rounds1=c(1,cumsum(rounds)[-length(rounds)]+1)
	rounds2=cumsum(rounds)
	
	# Merge 'points' and 'valid':
	points=NAs(length(uniqueplayers),max(rounds2))
	valid=!FALSEs(length(uniqueplayers),max(rounds2))
	rownames(points)=uniqueplayers
	rownames(valid)=uniqueplayers
	for(pl in seq_along(uniqueplayers)){
		
		for(s in seq_along(rounds)){
			thisplayer=which(uniqueplayers[pl]==players[[s]])
			if(length(thisplayer)>0){
				points[pl,rounds1[s]:rounds2[s]] = pointslist[[s]][thisplayer,]
				valid[pl,rounds1[s]:rounds2[s]] = validlist[[s]][thisplayer,]
				}
			}
		}
	
	players=rownames(points)
	
	which.deltakarar=which("Deltakarar"==players)
	
	neworder=c(seq_along(players)[-which.deltakarar],which.deltakarar)
		
	valid=valid[neworder,]
	# Do not add 0.1 to the Deltakarar:
	valid[nrow(valid),]=TRUE
	points=points[neworder,]
		
	dir="/Users/arnejohannesholmin/Documents/Diverse/Bordhockey/BSI Bordhockey/Bergensligaen/Ligastillingar"
	
	if(tolower(substr(type[1],1,1))=="g"){
		# Transform to places instead of points:
		points123=array(match(points,p),dim=dim(points))[-nrow(points),,drop=FALSE]
		row.names(points123)=row.names(points)[-nrow(points)]
		pointsgsb=t(apply(points123,1,tabulate,nbins=max(points123,na.rm=TRUE)))
		row.names(pointsgsb)=row.names(points)[-nrow(points)]
		pointsgsb=pointsgsb[order(apply(pointsgsb,1,function(x) sum(10^rev(seq_along(x))*x)),decreasing=TRUE),]
		pointsgsb=rbind(Spelar=c("G", "S","B",4:ncol(pointsgsb)),pointsgsb)
		
		nspace_tab=4
		ntab=floor(nchar(row.names(pointsgsb))/nspace_tab)+1
		maxntab=max(ntab)
		tabs=c("","\t","\t\t","\t\t\t","\t\t\t\t","\t\t\t\t\t","\t\t\t\t\t\t","\t\t\t\t\t\t\t")
		pointsgsb[pointsgsb==0]="."
		
	
		out=cbind(paste(row.names(pointsgsb),tabs[maxntab-ntab+1],sep=""),pointsgsb)
		out=apply(out,1,paste,collapse="\t",sep="")
		filergb=file.path(dir,paste("BL",paste(seasons,sep="",collapse=""),"_GSB.txt",sep="_"))
		writeLines(out,filergb,sep="\n")
		
		list(RGB=out,file=filergb)
		}
	else if(tolower(substr(type[1],1,1))=="p"){
		
		validpoints0=validpoints=points
		validpoints[!valid]=NA
		validpoints0[!valid]=0
		
		sumpoints=apply(validpoints0,1,function(x){x[is.na(x)]=0;cumsum(x)})
		if(length(dim(sumpoints))==0){
			dim(sumpoints)=c(1,length(sumpoints))
			}
		
		counts=rowSums(!is.na(points))
		max=apply(points,1,max,na.rm=TRUE)
		average=rowMeans(validpoints,na.rm=TRUE)
		totalsumpoints=rowSums(points,na.rm=TRUE)
		totalaverage=rowMeans(points,na.rm=TRUE)
		
		# Add 0.1 to identify not counted results:
		points[!valid & !is.na(points)]=points[!valid & !is.na(points)]+0.1
		
		points=cbind(points,sum=sumpoints[nrow(sumpoints),],count=counts,max,avg=average,sumT=totalsumpoints,avgT=totalaverage)
		points=round(points,digits=2)
		printt(colnames(points))
		
		orderpoints=order(sumpoints[nrow(sumpoints),seq_len(nrow(points)-1)],decreasing =TRUE)
		oldorder=order(sumpoints[nrow(sumpoints)-1,seq_len(nrow(points)-1)],decreasing =TRUE)
		points[-nrow(points),]=points[-nrow(points),][orderpoints,]
		rownames(points)[-nrow(points)]=rownames(points)[-nrow(points)][orderpoints]
		printt(colnames(points))
		
		Spelar=rownames(points)
		Klubb=c(uniqueplayersclubs[orderpoints],NA)
		points=cbind(Nr=c(seq_len(nrow(points)-1),NA),Forr=c(match(seq_along(oldorder),oldorder)[orderpoints],NA),Spelar=Spelar,Klubb=Klubb,points)
		printt(colnames(points))
		
		points=as.data.frame(points)
		points[,c(1:2,5:ncol(points))]=sapply(points[,c(1:2,5:ncol(points))],function(x) as.numeric(as.character(x)))
		points[,3]=as.character(points[,3])
		points[,4]=as.character(points[,4])
		
		
		#asnum=as.numeric(colnames(points))
		#isnum=!is.na(asnum)
		
		#filergb=file.path(dir,paste("BL_",paste(seasons,sep="_",collapse=""),".xls",sep=""))
		filergb=paste("BL_",paste(seasons,sep="_",collapse=""),".xls",sep="")
		
		printt(colnames(points))
		
		#colnames(points)[isnum]=order(asnum[isnum])
		colnames(points)[-c(1:4,ncol(points)+(-5:0))]=sequence(rounds)
		printt(colnames(points))
		
		list(points=points,file=filergb)
		}		
	}
