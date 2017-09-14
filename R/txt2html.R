#' txt2html
#' @param x
#'
#' @importFrom TSD chars
#' @importFrom tools file_ext
#'
#' @export
#'
txt2html=function(x,link=list(),outdir=NULL){
	if(file.info(x)$isdir){
		x=list.files(x,recursive=TRUE,full.names=TRUE)
		}
	# Accept only text files with the extention "txt":
	x=x[file_ext(x)=="txt"]
	
	title=strsplit(basename(dirname(x[1])),", ",fixed=TRUE)[[1]]
	date=title[1]
	title=title[2]
	html=NULL
	allres=list()
	allplayers=list()
	
	for(f in seq_along(x)){
		l=readLines(x[f])
		
		############################################################
		######### (1) Read the file as a group stage file: #########
		############################################################
		if(length(grep("tour",l,ignore.case=TRUE))>0 && length(grep("table",l,ignore.case=TRUE))>0){
			# Detect rounds:
			atTour=grep("Tour",substr(l,3,10),ignore.case=TRUE)
			roundnr=as.numeric(substr(l[atTour],1,2))
			groupnames=substring(l[atTour],unlist(gregexpr(", ",l[atTour]))+1)
			ugroupnames=unique(groupnames)
			groupnr=match(groupnames,ugroupnames)
			atRes=grep(":",l,fixed=TRUE)
			roundatplayer=findInterval(atRes,atTour)
			groupatplayer=groupnr[roundatplayer]
	
			# Get results:
			res=strsplit(l[atRes],":",fixed=TRUE)
			res=unlist(res)
			res=gsub("^\\s+|\\s+$", "", res)
			res=matrix(as.numeric(res),ncol=2,byrow=TRUE)
			allres=c(allres, list(res))
			
			# Get player names:
			players=matrix(l[outer(c(-3,-1),atRes,"+")],ncol=2,byrow=TRUE)
			# Abbreviate too long names so that no players have names longer than 22 characters:
			tooLong=which(nchar(players)>22)
			singlenames=strsplit(players[tooLong]," ")
			singlenames=unlist(lapply(singlenames,function(xx) paste(xx[1], " ", substr(xx[2],1,1), ". ", xx[-c(1,2)], sep="")))
			players[tooLong]=singlenames
			allplayers=c(allplayers, list(players))
			
			uplayers=unique(c(players))
			playerGroup=unique(cbind(c(players), groupatplayer))
			playerGroup=playerGroup[match(playerGroup[,1],uplayers),]
			playerGroup=data.frame(playerGroup[,1],as.numeric(playerGroup[,2]))
			playerGroup[,1]=as.character(playerGroup[,1])
			
			# Extract results and players:
			playersSorted=vector("list",length(uplayers))
			resSorted=vector("list",length(uplayers))
			matches=vector("list",length(uplayers))
			names(playersSorted)=uplayers
			names(resSorted)=uplayers
			names(matches)=uplayers
			
			for(i in seq_along(matches)){
				home=players[,1]==names(matches)[i]
				away=players[,2]==names(matches)[i]
				homeOrAway=home | away
				theseplayers=players[homeOrAway,,drop=FALSE]
				theseres=res[homeOrAway,,drop=FALSE]
				flip=theseplayers[,2]==names(matches)[i]
				theseres[flip,]=theseres[flip,,drop=FALSE][,2:1]
				theseplayers[flip,]=theseplayers[flip,,drop=FALSE][,2:1]
				# Store in the sorted player and res lists:
				resSorted[[i]]=theseres
				playersSorted[[i]]=theseplayers
				
				strings=chars(length(theseplayers[,1]))
				for(j in seq_along(theseplayers[,1])){
					ncharj=sum(nchar(theseplayers[j,]))
					strings[j]=paste(theseplayers[j,1]," - ",theseplayers[j,2],paste(rep(" ",l=45-ncharj),collapse=""), theseres[j,1],"-",theseres[j,2],sep="")
					}
				matches[[i]]=strings
				}
				
				
			# Generate table:
		 	TableHeader="<STRONG> Pl Navn                                         K    S  U  T   MS  MI  +/-    P"
		 	namesTable=c("Navn","K","S","U","T","MS","MI","pm","P")
		 	Table=vector("list",length(namesTable))
		 	names(Table)=namesTable
			#Table_Pl=seq_along(resSorted)
			Table$Navn=uplayers
			Table$S=sapply(resSorted,function(xx) sum(xx[,1]>xx[,2]))
			Table$U=sapply(resSorted,function(xx) sum(xx[,1]==xx[,2]))
			Table$T=sapply(resSorted,function(xx) sum(xx[,1]<xx[,2]))
			Table$K=Table$S+Table$U+Table$T
			Table$MS=sapply(resSorted,function(xx) sum(xx[,1]))
			Table$MI=sapply(resSorted,function(xx) sum(xx[,2]))
			Table$pm=getpm(resSorted)
			Table$P=getPoints(resSorted)
			ppp=Table$P + (playerGroup[,2]-1)*length(Table$P)
			equalPoints=table(ppp)
			equalPoints=as.numeric(names(equalPoints[equalPoints>1]))
			for(i in seq_along(equalPoints)){
				mutualPlayersInd=which(ppp==equalPoints[i])
				mutual=extractMutual(Table$Navn[mutualPlayersInd],playersSorted,resSorted)
				ppp[mutualPlayersInd]=ppp[mutualPlayersInd] + getPoints(mutual) * 1e-3 + getpm(mutual) * 1e-6 + Table$pm[mutualPlayersInd] * 1e-9
				}
			ppp=1e6-ppp
			
			# Split into groups:
			Table=lapply(Table,split,playerGroup[,2])
			Table$Pl=by(ppp,playerGroup[,2],rank, ties.method="first")
			Table$order=by(ppp,playerGroup[,2],order)
			
			
			Table$Lines=Table$Navn
			for(i in seq_along(Table$Lines)){
				Table$Lines[[i]]=paste(
					spacepadfirst(Table$Pl[[i]],2), 
					". ", 
					spacepadlast(Table$Navn[[i]],44),
					Table$K[[i]],
					spacepadfirst(Table$S[[i]],5),
					spacepadfirst(Table$U[[i]],3),
					spacepadfirst(Table$T[[i]],3),
					spacepadfirst(Table$MS[[i]],5),
					spacepadfirst(Table$MI[[i]],4),
					spacepadfirst(Table$pm[[i]],5),
					spacepadfirst(Table$P[[i]],5),sep="")
				Table$Lines[[i]]=Table$Lines[[i]][Table$order[[i]]]
				Table$Lines[[i]]=c(paste("<STRONG>Tabell ",ugroupnames[i],"</STRONG>",sep=""),"",TableHeader,paste("</STRONG>",Table$Lines[[i]][1],sep=""),Table$Lines[[i]][-1])
				}
			
			# Generate results and merge tables to the results:
			Results=split(matches,playerGroup[,2])
			for(i in seq_along(Table$Lines)){
				Results[[i]]=Results[[i]][Table$order[[i]]]
				Results[[i]]=c(
					Table$Lines[[i]],"",
					paste("<STRONG>Resultater ",ugroupnames[i],"</STRONG>",sep=""),
					unlist(lapply(Results[[i]],append,"",after=0),use.names=FALSE),
					if(i<length(Table$Lines)) c("","<HR>","") else ""
					)
				}
			Results=unlist(Results,use.names=FALSE)
			
			# Add to the output html file:
			html=c(html,"",Results)
			if(f<length(x)){
				html=c(html,"<HR>")
				}
			
			if(length(link)>0){
				for(linkind in seq_along(link)){
					# Add the results of a previous stage:
					if(link[[linkind]][2]==f){
						
						res=rbind(res,allres[[link[[linkind]][1]]])
						players=rbind(players,allplayers[[link[[linkind]][1]]])
						
						playersSorted=vector("list",length(uplayers))
						resSorted=vector("list",length(uplayers))
						matches=vector("list",length(uplayers))
						names(playersSorted)=uplayers
						names(resSorted)=uplayers
						names(matches)=uplayers
						
						for(i in seq_along(matches)){
							home=players[,1]==names(matches)[i]
							away=players[,2]==names(matches)[i]
							homeOrAway=home | away
							theseplayers=players[homeOrAway,,drop=FALSE]
							theseres=res[homeOrAway,,drop=FALSE]
							flip=theseplayers[,2]==names(matches)[i]
							theseres[flip,]=theseres[flip,,drop=FALSE][,2:1]
							theseplayers[flip,]=theseplayers[flip,,drop=FALSE][,2:1]
							
							# Add the criterion that the opponent must be in the correct playerGroup:
							thisPlayerGroup=playerGroup[playerGroup[,1]==names(matches)[i],2]
							opponentPlayerGroup=playerGroup[match(theseplayers[,2],playerGroup[,1]),2]
							
							theseres=theseres[which(opponentPlayerGroup==thisPlayerGroup),]
							theseplayers=theseplayers[which(opponentPlayerGroup==thisPlayerGroup),]
							
							
							# Store in the sorted player and res lists:
							resSorted[[i]]=theseres
							playersSorted[[i]]=theseplayers
							
							strings=chars(length(theseplayers[,1]))
							for(j in seq_along(theseplayers[,1])){
								ncharj=sum(nchar(theseplayers[j,]))
								strings[j]=paste(theseplayers[j,1]," - ",theseplayers[j,2],paste(rep(" ",l=45-ncharj),collapse=""), theseres[j,1],"-",theseres[j,2],sep="")
								}
							matches[[i]]=strings
							}
							
						# Generate table:
					 	TableHeader="<STRONG> Pl Navn                                         K    S  U  T   MS  MI  +/-    P"
					 	namesTable=c("Navn","K","S","U","T","MS","MI","pm","P")
					 	Table=vector("list",length(namesTable))
					 	names(Table)=namesTable
						#Table_Pl=seq_along(resSorted)
						Table$Navn=uplayers
						Table$S=sapply(resSorted,function(xx) sum(xx[,1]>xx[,2]))
						Table$U=sapply(resSorted,function(xx) sum(xx[,1]==xx[,2]))
						Table$T=sapply(resSorted,function(xx) sum(xx[,1]<xx[,2]))
						Table$K=Table$S+Table$U+Table$T
						Table$MS=sapply(resSorted,function(xx) sum(xx[,1]))
						Table$MI=sapply(resSorted,function(xx) sum(xx[,2]))
						Table$pm=getpm(resSorted)
						Table$P=getPoints(resSorted)
						ppp=Table$P + (playerGroup[,2]-1)*length(Table$P)
						equalPoints=table(ppp)
						equalPoints=as.numeric(names(equalPoints[equalPoints>1]))
						for(i in seq_along(equalPoints)){
							mutualPlayersInd=which(ppp==equalPoints[i])
							mutual=extractMutual(Table$Navn[mutualPlayersInd],playersSorted,resSorted)
							ppp[mutualPlayersInd]=ppp[mutualPlayersInd] + getPoints(mutual) * 1e-3 + getpm(mutual) * 1e-6 + Table$pm[mutualPlayersInd] * 1e-9
							}
						ppp=1e6-ppp
						
						# Split into groups:
						Table=lapply(Table,split,playerGroup[,2])
						Table$Pl=by(ppp,playerGroup[,2],rank, ties.method="first")
						Table$order=by(ppp,playerGroup[,2],order)
						
						
						Table$Lines=Table$Navn
						for(i in seq_along(Table$Lines)){
							Table$Lines[[i]]=paste(
								spacepadfirst(Table$Pl[[i]],2), 
								". ", 
								spacepadlast(Table$Navn[[i]],44),
								Table$K[[i]],
								spacepadfirst(Table$S[[i]],5),
								spacepadfirst(Table$U[[i]],3),
								spacepadfirst(Table$T[[i]],3),
								spacepadfirst(Table$MS[[i]],5),
								spacepadfirst(Table$MI[[i]],4),
								spacepadfirst(Table$pm[[i]],5),
								spacepadfirst(Table$P[[i]],5),sep="")
							Table$Lines[[i]]=Table$Lines[[i]][Table$order[[i]]]
							Table$Lines[[i]]=c(paste("<STRONG>Tabell ",ugroupnames[i]," (merged) </STRONG>",sep=""),"",TableHeader,paste("</STRONG>",Table$Lines[[i]][1],sep=""),Table$Lines[[i]][-1])
							}
							
						# Generate results and merge tables to the results:
						#Results=split(matches,playerGroup[,2])
						Results=list()
						for(i in seq_along(Table$Lines)){
							#Results[[i]]=Results[[i]][Table$order[[i]]]
							Results[[i]]=c(
								Table$Lines[[i]],"",
								#paste("<STRONG>Resultater ",ugroupnames[i],"</STRONG>",sep=""),
								#unlist(lapply(Results[[i]],append,"",after=0),use.names=FALSE),
								if(i<length(Table$Lines)) c("","<HR>","") else ""
								)
							}
						Results=unlist(Results,use.names=FALSE)
						
						# Add to the output html file:
						html=c(html,"",Results)
						if(f<length(x)){
							html=c(html,"<HR>")
							}
							
							
						}
					}
				}
			}
		
		############################################################
		########### (2) Read the file as a play-off file: ##########
		############################################################
		else{
			atRounds = which(l=="Players")
			atNumMatches = which(l=="Score")
			printt(atNumMatches)
			roundTypes = l[atRounds-2]
			roundNumMatches = as.numeric(l[atNumMatches-1])
			printt(roundNumMatches)
			atGames = which(l=="-")
			roundLevel = findInterval(atGames,atRounds)
			NumMatches = roundNumMatches[roundLevel]
			printt(NumMatches)
			players=matrix(l[c(outer(c(-1,1),atGames,"+"))],byrow=TRUE,ncol=2)
			totalRes=l[atGames+NumMatches+2]
			# Account for the number of matches (best of 1, 3, 5, 7):
			atRes = 1+sequence(NumMatches)+rep(atGames,NumMatches)
			singleRes=split(l[atRes],rep(seq_along(atGames),NumMatches))
			#singleRes=split(l[c(outer(2:8,atGames,"+"))],rep(seq_along(atGames),each=7))
			# Convert ":" to "-", and " (OT)" to "s"
			totalRes=gsub(":","-",totalRes,fixed=TRUE)
			singleRes=lapply(singleRes, gsub, pattern=":", replacement="-",fixed=TRUE)
			singleRes=lapply(singleRes, gsub, pattern=" (OT)", replacement="s",fixed=TRUE)
			# Remove not played matches:
			singleRes=lapply(singleRes, function(xx) xx[nchar(xx)>0])
			
			playoffMatches=chars(nrow(players))
			for(i in seq_along(playoffMatches)){
				playoffMatches[i]=paste(spacepadlast(paste(players[i,],collapse=" - "),48),totalRes[i]," (",paste(singleRes[[i]],collapse=","),")",sep="")
				}
			# Insert empty lines and stages:
			playoffMatches=split(playoffMatches,roundLevel)
			for(i in seq_along(playoffMatches)){
				playoffMatches[[i]]=c(paste("<STRONG>",roundTypes[i],":</STRONG>",sep=""),playoffMatches[[i]],"")
				}
			playoffMatches=unlist(playoffMatches,use.names=FALSE)
			
			# Add header lines at the top:
			playoffMatches=c(paste("<STRONG>Sluttspill</STRONG>",sep=""),"",playoffMatches)
			
			# Add to the output html file:
			html=c(html,"",playoffMatches)
			if(f<length(x)){
				html=c(html,"<HR>")
				}
			}
		}
	html=c(htmlHeader(title, date),html,htmlEnd())
	
	#Encoding(html)<-"latin1"
	filename=paste(date,", ",title,".html",sep="")
	if(length(outdir)==0){
		f=file.path(dirname(x[1]),filename)
		}
	else{
		f=file.path(outdir,filename)
		}
	f=file(f,open="w",encoding="latin1")
	writeLines(html,f)
	close(f)
	# Output
	invisible(html)
	}
