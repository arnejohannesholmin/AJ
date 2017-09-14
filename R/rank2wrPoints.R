#*********************************************
#*********************************************
#' Calculates WR points based on best scores.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD zeros
#'
#' @export
#' @rdname rank2wrPoints
#'
rank2wrPoints=function(best,level=5,bronze=TRUE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-12-20 - Clean version.
	########### DESCRIPTION: ###########
	# Calculates WR points based on best scores.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'best' is a vector of the best scores of the players.
	# - 'level' is the level of the tournament.
	
	
	##################################################
	##################################################
	##### Preparation #####
	lbest=length(best)
	if(lbest<4){
		return(zeros(lbest))
		}
	scales=c(1,0.99,0.98,0.9,0.7,0.4)
	m=c(1000,100,100,70,40,20)
	
	
	##### Execution #####
	# Apply the rules of the WR:
	atleast1=floor(m[level]/(2^(seq_along(best)-1)))
	atleast2=rev(seq_along(best))
	
	# Calculate the points based on the WR:
	best=sort(best,decreasing=TRUE)
	# 'x' is the maxpoints of each player:
	p=best
	for(i in seq_len(lbest-1)){
		p[i]=sum(best[seq(i,min(lbest,i+3))],na.rm=TRUE)/4*scales[level]
		}
	p[lbest]=1
	p=round(p)
	# atleast1[lbest]=1
	if(lbest>=4 && !bronze){
		p[3:4]=round(mean(p[3:4]))
		}
	
	
	##### Output #####
	apply(cbind(p,atleast1,atleast2),1,max)
	##################################################
	##################################################
	}
