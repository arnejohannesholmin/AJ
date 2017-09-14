monrad <- function(pl, n, rank, start=c("random", "rank", "brackets")){
	if(odd(length(pl))){
		pl <- c(pl, NA)
		rank <- c(rank, NA)
	}
	npl <- length(pl)
	
	rounds <- vector("list", n)
	startorder <- NULL
	# First round:
	if(start[1]=="random"){
		startorder <- sample(npl)
	}
	else if(start[1]=="rank"){
		startorder <- sample(npl)
	}
	else if(start[1]=="brackets"){
		
	}
	rounds[[1]] <- matrix(pl[startorder], ncol=2, byrow=TRUE)
	
	}