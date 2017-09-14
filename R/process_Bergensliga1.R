#*********************************************
#*********************************************
#' ***DECRIPTION MISSING***
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD NAs
#'
#' @export
#' @rdname process_Bergensliga1
#'
process_Bergensliga1=function(season="H2011",p,pKM){

	# Create table for the Bergensliga:
	dir=file.path("/Users/arnejohannesholmin/Documents/Diverse/Bordhockey/BSI Bordhockey/Bergensligaen/Ligastillingar/Files",season)
	f=list.files(dir,full.names=TRUE)
	
	dates=NAs(length(f))
	names=vector("list",length(f))
	clubs=vector("list",length(f))
	type=vector("list",length(f))
	for(i in seq_along(f)){
		suppressWarnings(a<-readfromWR(f[i]))
		dates[i]=a$dates
		names[[i]]=a$names
		clubs[[i]]=a$clubs
		type[[i]]=a$type
		}
	# Order chronologically:
	names=names[order(dates)]
	clubs=clubs[order(dates)]
	type=type[order(dates)]
	dates=dates[order(dates)]
	
	
	fromlast=rev(!duplicated(rev(unlist(names))))
	players=unlist(names)[fromlast]
	playersclubs=unlist(clubs)[fromlast]
	
	points=NAs(length(players),length(f))
	rownames(points)=players
	for(i in seq_along(f)){
		if(type[[i]]=="Bergenslig"){
			points[names[[i]],i]=p[seq_along(names[[i]])]
			}
		else if(type[[i]]=="StudentMes"){
			points[names[[i]],i]=pKM[seq_along(names[[i]])]
			}
		}
		
	colnames(points)=dates
	# Add number of participants:
	list(points=rbind(points,Deltakarar=apply(points,2,function(x) sum(!is.na(x)))),players=players,playersclubs=playersclubs)
	}
