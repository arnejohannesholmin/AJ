#' extractMutual
#' @param x
#' @export
#'
extractMutual=function(players,playersSorted,resSorted){
	playersind=names(playersSorted)%in%players
	playersSorted=playersSorted[playersind]
	resSorted=resSorted[playersind]
	for(i in seq_along(playersSorted)){
		playersind=playersSorted[[i]][,2]%in%players
		#playersSorted[[i]]=playersSorted[[i]][playersind,]
		resSorted[[i]]=resSorted[[i]][playersind,,drop=FALSE]
		}
	resSorted
	}
