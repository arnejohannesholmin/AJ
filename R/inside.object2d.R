#*********************************************
#*********************************************
#' Returns a logical vector for whether the 2D positions given in 'x' are inside the object specified by 'object', 'center', 'angle' and 'par'. 
#'
#' @param x  is a three column matrix representing the x- y- and z-coordinates. If x is a vector, it is considered to be one single 2D point.
#' @param object  is string representing the type of object to use. Currently implemented are "ellipsoid" and "cuboid" (may be abbreviated).
#' @param par  is a vector of three elements representing the semi axis lengths for ellipsoid and the x-width, y-depth and z-height of cuboid.
#' @param center  is a vector of three elements representing the centre position of the object.
#' @param angle  is the angle of the major axis of the ellipsoid in the x-y-plane, in the case that object=="ellipsoid".
#' @param logical  is TRUE if the output should be locigal instead of numeric.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname inside.object2d
#'
inside.object2d<-function(x,object=c("ellipse","rect"),par=c(1,1),center=c(0,0),angle=0,logical=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-04-26 - Clean version.
	########### DESCRIPTION: ###########
	# Returns a logical vector for whether the 2D positions given in 'x' are inside the object specified by 'object', 'center', 'angle' and 'par'. 
	########## DEPENDENCIES: ###########
	# 
	############ VARIABLES: ############
	# ---x--- is a three column matrix representing the x- y- and z-coordinates. If x is a vector, it is considered to be one single 2D point.
	# ---object--- is string representing the type of object to use. Currently implemented are "ellipsoid" and "cuboid" (may be abbreviated).
	# ---par--- is a vector of three elements representing the semi axis lengths for ellipsoid and the x-width, y-depth and z-height of cuboid.
	# ---center--- is a vector of three elements representing the centre position of the object.
	# ---angle--- is the angle of the major axis of the ellipsoid in the x-y-plane, in the case that object=="ellipsoid".
	# ---logical--- is TRUE if the output should be locigal instead of numeric.
		
	
	##################################################
	##################################################
	##### Preparation #####
	if(length(dim(x))!=2){
		x=array(x[1:2],dim=c(1,2))
		}
	par=matrix(par,ncol=2,nrow=nrow(x),byrow=TRUE)
	center=matrix(center,ncol=2,nrow=nrow(x),byrow=TRUE)
	
	
	##### Execution and output #####
	if(tolower(substr(object[1],1,1))=="e"){
		x=x-center
		# Negative angle because the rotation is performed on the points, ang rotated to the angle of the object:
		if((angle %% (2*pi)) != 0){
			x=rotate(x,by="z",ang=-angle)
			}
		if(logical){
			rowSums(x^2/par^2)<=1
			}
		else{
			which(rowSums(x^2/par^2)<=1)
			}
		}
	else if(tolower(substr(object[1],1,1))=="r"){
		x=x-center
		if(logical){
			abs(x[,1])<=par[,1] & abs(x[,2])<=par[,2]
			}
		else{
			which(abs(x[,1])<=par[,1] & abs(x[,2])<=par[,2])
			}
		}
	else{
		stop("Unknown object")
		}
	##################################################
	##################################################
	}
