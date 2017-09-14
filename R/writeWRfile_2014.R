#*********************************************
#*********************************************
#' ***DECRIPTION MISSING***
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD NAs ones
#'
#' @export
#' @rdname writeWRfile_2014
#'
writeWRfile_2014=function(players,tournament,date,series,A=NULL,bronze=TRUE,info=list(),dir=NULL, euro=FALSE){
	
	if(length(grep("/",tournament,fixed=TRUE))>0){
		warning("The file name cannot contain the character \"/\" (changed to \"-\")")
		tournament=gsub("/","-",tournament)
		}
	
	if(length(dir)==0){
		dir=file.path(Sys.getenv("HOME"),"Documents/Diverse/Bordhockey/FantasyNBA/Tournaments")
		}
	thisdate=rev(strsplit(as.character(date),".",fixed=TRUE)[[1]])
	thisdate[nchar(thisdate)==1]=paste("0",thisdate[nchar(thisdate)==1],sep="")
	thisdate=paste(thisdate,collapse="")
	
	out=data.frame(a=players,b=rep("-",length=length(players)),c=rep("-",length=length(players)),d=rep("-",length=length(players)),e=ones(length(players)))
	out[,1]=as.character(out[,1])
	out[,2]=as.character(out[,2])
	out[,3]=as.character(out[,3])
	WR=getWR_2014(date=thisdate)
		
	
	if(length(info)>0 && sort(names(info))==c("Club","Nation","Player") && length(info$Club)==length(info$Nation) && length(info$Nation)==length(info$Player)){
		
		thisadd=data.frame(Rank=NAs(length(info$Player)),ID_Player=NAs(length(info$Player)),Player=info$Player,Club=info$Club,Nation=info$Nation,Points=NAs(length(info$Player)),Best=NAs(length(info$Player)))
		
		WR = rbind(WR,thisadd)
		}
	
	if(!all(players %in% WR$Player)){
		missing=setdiff(players,WR$Player)
		rank=match(missing,players)
		missing=paste(missing," (",rank,")",sep="")
		warning(paste("The following players were not found in among the registered players (used the input 'info' to add, in a two column matrix, the club and nation for each player (nation given in 3 capital letters)):",paste(missing,collapse="\n"),sep="\n"))
		}
	
	WR=WR[WR$Player %in% players,]
	out[match(WR$Player,players),2]=as.character(WR$Club)
	out[match(WR$Player,players),3]=as.character(WR$Nation)
	out[match(WR$Player,players),5]=WR$Best
	
	
	level=getLevel(tournament)
	
	p=rank2wrPoints_2014(out[,5],level=level, euro=euro)
	
	Rank=seq_along(players)
	if(!bronze){
		Rank[4]=3
		p[3:4]=floor(mean(p[3:4]))
		}

	
	data=c("","","",""," 	Home	Ranking	Calendar	Players	Clubs	Nations	Series	News	 ",tournament,paste(c("Date","Series","City","Web","Participants","Level","Value"),c(date,series,"-","-","-",level,"-"),sep="\t"),"","","Final table","Pos. 5	Rank	Change	Player	Club	Nation	Rank points",paste(Rank,"","_",out[,1],out[,2],out[,3],p,sep="\t"),"ranking valid before the tournament and rank change caused by this tournament are stated","Colours explanation","junior	veteran	lady	lady junior	lady veteran","","")
	# Write the WR-file:
	if(length(A)){
		file=paste(dir,"/",tournament,"_A_",A,sep="")
		}
	else{
		file=paste(dir,tournament,sep="/")
		}
	writeLines(data,file)
	cat(paste("File",file,"written","\n"))
	}
