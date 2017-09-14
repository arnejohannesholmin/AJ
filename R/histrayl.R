#*********************************************
#*********************************************
#' Plots a histogram of 'x' added a line of the maximum likelihood fitted Rayleigh distribution to 'x'.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname histrayl
#'
histrayl<-function(x,sigma=NULL,breaks = "Sturges",coll="red",lengthx=1000,lwd=1.5,add=FALSE,plot=TRUE,...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	# Last: 2009-05-05 - Dependency on the package 'VGAM' removed, according to the implementatino of drayl(), prayl(), qrayl() and rrayl().
	########### DESCRIPTION: ###########
	# Plots a histogram of 'x' added a line of the maximum likelihood fitted Rayleigh distribution to 'x'.
	########## DEPENDENCIES: ###########
	# mlerayl(), drayl()
	############ VARIABLES: ############
	# - 'x' is the data.
	# - 'mu' and 'sigma' are the parameters in the Rayleigh distribution.
	# - 'coll' is the colour for the Rayleigh line.
	# - 'lengthx' is the length of the Rayleigh line.
	# - 'lwd' is the line width of the Rayleigh line.
	# - 'add' is TRUE if the line Rayleigh is to be added to a plot.
	# - '...' variables passed on to hist().
	
	
	##################################################
	##################################################
	##### Preparation #####
	if(is.null(sigma)){
		sigma=mlerayl(x,na.rm=TRUE)
		}
	
	
	##### Execution and output #####
	raylx=seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),length.out=lengthx)
	rayly=drayl(raylx,sigma)
	if(!add){
		hh=hist(x,plot=FALSE,breaks=breaks,...)
		# Adjusting for the total area of the histogram:
		area=sum(hh$counts*diff(hh$breaks))
		hh$counts=hh$counts/area
		if(plot){
			plot(hh,ylim=range(c(hh$density,rayly)))
			}
		# Reverting hh to its original scale:
		hh$counts=hh$counts*area
		}
	else{
		hh=hist(x,plot=FALSE,breaks=breaks,...)
		}
	if(plot){
		lines(raylx,rayly,col=coll,lwd=lwd,...)
		}
	invisible(list(hist=hh,raylx=raylx,rayly=rayly,sigma=sigma))
	##################################################
	##################################################
	}
