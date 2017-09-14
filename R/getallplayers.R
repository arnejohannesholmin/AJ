#*********************************************
#*********************************************
#' Returns a data frame of all players in the FantasyNBA.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD NAs
#'
#' @export
#' @rdname getallplayers
#'
getallplayers<-function(){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-12-20 - Clean version.
	########### DESCRIPTION: ###########
	# Returns a data frame of all players in the FantasyNBA.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'x' is the file to read.
	
	
	##################################################
	##################################################
	##### Preparation and execution #####
	addnames=readLines(file.path(Sys.getenv("HOME"),"Documents/Diverse/Bordhockey/FantasyNBA/Rankings/Added players"))
	removenames=readLines(file.path(Sys.getenv("HOME"),"Documents/Diverse/Bordhockey/FantasyNBA/Rankings/Removed players"))
	
	dirTournaments=file.path(Sys.getenv("HOME"),"Documents/Diverse/Bordhockey/FantasyNBA/Tournaments")
	f=list.files(dirTournaments,full.names=TRUE)
	# Read the tournaments:
	tournamentnames=NAs(length(f))
	playersplayed=vector("list",length(f))
	nationsplayed=vector("list",length(f))
	clubsplayed=vector("list",length(f))
	for(i in seq_along(f)){
		suppressWarnings(a<-readfromWR(f[i]))
		playersplayed[[i]]=gsub("\"","",a$names)
		nationsplayed[[i]]=a$nations
		clubsplayed[[i]]=a$clubs
		}
	
	# Read all the WR-files and register all norwegian players:
	dir=file.path(Sys.getenv("HOME"),"Documents/Diverse/Bordhockey/FantasyNBA/Rankings")
	f=list.files(dir,full.names=TRUE)
	rankingfiles=f[substr(basename(f),1,7)=="ranking"]
	# rankingdate=as.numeric(paste(substr(basename(rankingfiles),8,11),substr(basename(rankingfiles),13,14),substr(basename(rankingfiles),16,17),sep=""))
	players=vector("list",length(rankingfiles))
	nations=vector("list",length(rankingfiles))
	clubs=vector("list",length(rankingfiles))
	for(i in seq_along(rankingfiles)){
		# Read the ranking file:
		dd=read.table(rankingfiles[i],skip=1,header=TRUE,sep="\t")
		# Get player names, clubs and nations:
		players[[i]]=levels(dd$Player)[dd$Player]
		clubs[[i]]=levels(dd$Club)[dd$Club]
		nations[[i]]=levels(dd$Nation)[dd$Nation]
		}
	
	players=c(players,playersplayed)
	clubs=c(clubs,clubsplayed)
	nations=c(nations,nationsplayed)
	
	# Unlist the lists:
	players=unlist(players)
	d=duplicated(players)
	
	players=players[!d]
	clubs=unlist(clubs)[!d]
	nations=unlist(nations)[!d]
	
	# Get the norwegian and the approved players and uniquify:
	norwegian=nations=="NOR"
	disp=players %in% addnames
	not=players %in% removenames
	norwegian = (norwegian | disp) & !not
	
	
	##### Output #####
	cbind(players=players[norwegian],clubs=clubs[norwegian],nations=nations[norwegian])
	##################################################
	##################################################
	}
