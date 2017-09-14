#*********************************************
#*********************************************
#' Periodic parametric regression on a periodic response variable.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD zeros
#'
#' @export
#' @rdname perreg
#'
perreg<-function(x,y=NULL,d=NULL,m=NULL){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2008-07-28 - Finished.
	# Update:  2009-02-18 - Expanded to treating several periods and returning intersect.
	# Last:  2009-07-31 - Cleaned up and effectuated (dependence on zipper() removed).
	########### DESCRIPTION: ###########
	# Periodic parametric regression on a periodic response variable.
	########## DEPENDENCIES: ###########
	# zeros()
	############ VARIABLES: ############
	# - 'x' and 'y' are the x-coordinates and y-coordinates of the input trace, given as separate vectors 'x' and 'y', or as a list or matrix 'x' holding both the x-coordinates and the y-coordinates, or if y==NULL 'x' is interpreted as 'y' and x=seq_along(x).
	# - 'd' is a vector of periods predefined i the model.
	# - 'm' is the intersect the input time response variable.
	
	
	##################################################
	##################################################
	##### Preparation #####
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
	
	# The period 'd' is essential to the regression, and a warning and NULL is returned if 'd' is not given:
	if(is.null(d)){
		stop("d not given")
		}
	ld=length(d)
	
	# If the intersect 'm' is not given:
	if(is.null(m)){
		m=mean(y)
		}
	
	
	##### Execution #####
	# Subtracting the intercect:
	y=y-m
	# The regression is parametric and is described in "~/Documents/Produktivt/R/Functions/Documentation/Periodic regression/periodic regression.pdf":
	# Building a matrix with cosine and sine column vectors alternating:
	z=zeros(lx,2*ld)
	z[,1:ld*2-1]=cos(2*pi*x%*%t(1/d))
	z[,1:ld*2]=sin(2*pi*x%*%t(1/d))
	# *Solving the regression least squares:
	betahatt<-solve(t(z)%*%z)%*%t(z)%*%y
	# Extracting amplitude and phase:
	Aphihatt=matrix(betahatt,ncol=2,byrow=TRUE)
	Ahatt<-sqrt(Aphihatt[,2]^2+Aphihatt[,1]^2)
	phihatt<-atan2(Aphihatt[,2],Aphihatt[,1])
	
	
	##### Execution and output #####
	list(m=m,A=Ahatt,phi=phihatt)
	##################################################
	##################################################
	}
