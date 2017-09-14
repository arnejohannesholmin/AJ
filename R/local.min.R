#*********************************************
#*********************************************
#' Locates local minima of a vector 'y' (given the vector 'x' of equal length). Method based on the definition given in Calculus, Fourth Edition by Robert A. Adams: (local minima occur iff there exists a h>0 so that y(x*)<=y(x) at all points |x*-x|<h).
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname local.min
#'
local.min<-function(x,y=NULL,strict=TRUE,w=1){
		
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2008-04-01 - First verdion with for loops.
	# Update: 2009-03-08 - Replaced witha a method including diff() performed twice (ind=diff(diff(x)>0)<0). Produced inaccurate results.
	# Update: 2009-03-08 - New method considering the sign of diff(x). Slightly slower than the previous version, but accurate.
	# Last:  2011-12-18 - Added the parameters 'strict' and 'w'.
	########### DESCRIPTION: ###########
	# Locates local minima of a vector 'y' (given the vector 'x' of equal length). Method based on the definition given in Calculus, Fourth Edition by Robert A. Adams: (local minima occur iff there exists a h>0 so that y(x*)<=y(x) at all points |x*-x|<h).
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'x' and 'y' are the x-coordinates and y-coordinates of the input, given as separate vectors 'x' and 'y', or as a list or matrix 'x' holding both the x-coordinates and the y-coordinates, or if y==NULL 'x' is interpreted as 'y' and x=seq_along(x).
	# - 'strict' is FALSE if shoulder points are to be classified as minima.
	# - 'w' is a width parameter causing minima separated by less or equal to 'w' to be replaced by the minimum of the two.
	
	
	##################################################
	##################################################
	##### Preparation #####
	listinputx=FALSE
	# Support for vector, matrix and list input for 'x':
	if(is.list(x)){
		names(x)=tolower(names(x))
		if(!is.null(x$x) && !is.null(x$y)){
			y=x$y
			x=x$x
			}
		else{
			y=x[[2]]
			x=x[[1]]
			}
		listinputx=TRUE
		}
	else if(is.null(y)){
		dimx=dim(x)
		if(length(dimx)==2){
			if(dimx[2]==1){
				y=drop(x)
				x=seq_along(x)
				}
			else{
				y=x[,2]
				x=x[,1]
				}
			}
		# Add zeros for the 'y' values:
		else if(is.null(dimx)){
			y=x
			x=seq_along(x)
			}
		else{
			stop("Invalid input")
			}
		}
	# 'x' and 'y' need to have equal length:
	lx=length(x)
	ly=length(y)
	if(lx!=ly){
		stop("'x' and 'y' lengths differ")
		}
	if(is.unsorted(x)){
		orderx=order(x)
		x=x[orderx]
		y=y[orderx]
		}
	
	
	##### Execution #####
	# 'ind' gives the degree of turn at each of the interior points of x (-2: (upp-down), -1: (upp-flat or flat-down), 0: (up-up, flat-flat or down-down), 1: (down-flat or flat-up) and 2: (down-up)):
	ind=diff(sign(diff(y)))
	# End points cannot be defined as local maxima, and FALSE is added to each end:
	if(!strict){
		ind=1+which(ind>0)
		}
	else{
		ind=1+which(ind>1)
		}
	
	tooclose=which(diff(x[ind])<=w)
	for(i in seq_along(tooclose)){
		if(!any(is.na(ind[tooclose[i]+0:1]))){
			ind[tooclose[i]+0:1][which.min(y[ind[tooclose[i]+0:1]])]=NA
			}
		}
	ind=ind[!is.na(ind)]
	
	
	##### Output #####
	cbind(x=x[ind],y=y[ind])
	##################################################
	##################################################
	}
